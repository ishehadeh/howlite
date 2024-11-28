use core::str;

use proptest::{
    prelude::{any, Just, Rng, Strategy},
    strategy::{self, ValueTree},
    string::string_regex,
};
use unicode_properties::{GeneralCategory, GeneralCategoryGroup, UnicodeGeneralCategory};

pub fn arbitrary_string_without_unicode_escapes() -> impl Strategy<Value = String> {
    string_regex(r#"[\pL\pM\pN\pS\pP\p{Zs}]*"#).expect("failed to compile regex")
}

pub fn arbitrary_simple_escape(for_string: bool) -> impl Strategy<Value = String> {
    if for_string {
        string_regex(r#"\\["nrt]"#).expect("failed to compile regex")
    } else {
        string_regex(r#"\\['nrt]"#).expect("failed to compile regex")
    }
}

pub fn arbitrary_ascii_escape() -> impl Strategy<Value = String> {
    string_regex("\\x[0-9a-fA-F]{2}").expect("failed to compile regex")
}

#[derive(Clone, Debug, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct CharFormatCase {
    // | 13..=15      |    6..=10                      |            0..=5      |
    //  num chars / 2      start bitfield                     uppercase bitfield
    packed: u16,
}

#[allow(
    clippy::unusual_byte_groupings,
    reason = "we use grouping here to represent fields in the packed struct"
)]
impl CharFormatCase {
    pub fn new(init: u8, chars: usize) -> CharFormatCase {
        assert!((2..=6).contains(&chars));
        let init_subset = init | !(0xffu8 << chars);
        CharFormatCase {
            packed: (((chars / 2) as u16) << 13) | ((init as u16) << 6) | (init_subset as u16),
        }
    }

    pub fn current_bitfield(&self) -> u8 {
        (self.packed & 0b000_0_000000_111111u16) as u8 & self.all_uppercase()
    }

    pub fn initial_bitfield(&self) -> u8 {
        ((self.packed & 0b000_0_111111_000000u16) >> 6) as u8 & self.all_uppercase()
    }

    pub fn length(&self) -> u8 {
        2 * ((self.packed & 0b111_0_000000_000000u16) >> 13) as u8
    }

    pub fn all_uppercase(&self) -> u8 {
        !(0xffu8 << self.length())
    }

    pub fn incr(&mut self) -> bool {
        if self.current_bitfield() == self.all_uppercase() {
            if self.initial_bitfield() == 0 {
                false
            } else {
                // wrap around to 0
                self.packed &= 0b111_1_111111_000000u16;
                true
            }
        } else if self.initial_bitfield() == self.current_bitfield() + 1 {
            false
        } else {
            self.packed += 1;
            true
        }
    }

    pub fn decr(&mut self) -> bool {
        if self.initial_bitfield() == self.current_bitfield() {
            false
        } else if self.current_bitfield() == 0 {
            // wrap around to max
            self.packed |= 0b000_0_000000_111111u16;
            true
        } else {
            self.packed -= 1;
            true
        }
    }

    pub fn reset(&mut self, len: u8) {
        self.packed &= 0b000_1_111111_000000u16;
        self.packed |= self.initial_bitfield() as u16;
        self.packed |= (len as u16 / 2) << 13;
    }

    pub fn is_uppercase(&self, ind: usize) -> bool {
        self.current_bitfield() & ((1 << ind) as u8) != 0
    }
}

impl ValueTree for CharFormatCase {
    type Value = Self;

    fn current(&self) -> Self::Value {
        *self
    }

    fn simplify(&mut self) -> bool {
        self.incr()
    }

    fn complicate(&mut self) -> bool {
        self.decr()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum CharFormat {
    Literal,
    SimpleEsc,
    Ascii,
    Unicode4,
    Unicode6,
}

impl CharFormat {
    pub fn all_by_complexity() -> [Self; 5] {
        [
            CharFormat::Unicode6,
            CharFormat::Unicode4,
            CharFormat::Ascii,
            CharFormat::SimpleEsc,
            CharFormat::Literal,
        ]
    }
    pub fn can_repr(&self, string: bool, c: char) -> bool {
        match self {
            CharFormat::Literal => {
                if string && matches!(c, '\"' | '\n' | '\t' | '\r') {
                    false
                } else if !string && matches!(c, '\'' | '\n' | '\t' | '\r') {
                    false
                } else {
                    let categ_group = c.general_category_group();
                    matches!(
                        categ_group,
                        GeneralCategoryGroup::Letter
                            | GeneralCategoryGroup::Mark
                            | GeneralCategoryGroup::Number
                            | GeneralCategoryGroup::Separator
                    ) || c.general_category() == GeneralCategory::SpaceSeparator
                }
            }
            CharFormat::Ascii => c as u32 <= 0xFF,
            CharFormat::Unicode4 => c as u32 <= 0xFFFF,
            CharFormat::Unicode6 => c as u32 <= 0xFFFFFF,
            CharFormat::SimpleEsc if string => matches!(c, '\"' | '\n' | '\t' | '\r'),
            CharFormat::SimpleEsc => matches!(c, '\'' | '\n' | '\t' | '\r'),
        }
    }

    fn complexity_index(&self) -> usize {
        match self {
            CharFormat::Literal => 4,
            CharFormat::SimpleEsc => 3,
            CharFormat::Ascii => 2,
            CharFormat::Unicode4 => 1,
            CharFormat::Unicode6 => 0,
        }
    }
}

pub struct CharReprValueTree<const IS_STRING: bool> {
    c: char,
    fmt: CharFormat,
    init_fmt: CharFormat,
    case: CharFormatCase,
    buf: [u8; 8],
}

impl<const IS_STRING: bool> CharReprValueTree<IS_STRING> {
    fn case_matters_for_format(&self) -> bool {
        matches!(
            self.fmt,
            CharFormat::Unicode4 | CharFormat::Unicode6 | CharFormat::Ascii
        )
    }

    fn simplify_format(&mut self) -> bool {
        let waterfall_index = self.fmt.complexity_index();
        let waterfall = CharFormat::all_by_complexity();
        for fmt in waterfall
            .into_iter()
            .cycle()
            .skip(waterfall_index + 1)
            .take_while(|&f| f != self.init_fmt)
        {
            if fmt.can_repr(IS_STRING, self.c) {
                self.fmt = fmt;
                match fmt {
                    CharFormat::Ascii => self.case.reset(2),
                    CharFormat::Unicode4 => self.case.reset(2),
                    CharFormat::Unicode6 => self.case.reset(6),
                    _ => (),
                };
                return true;
            }
        }
        false
    }

    fn complicate_format(&mut self) -> bool {
        let waterfall_index = self.fmt.complexity_index();
        let waterfall = CharFormat::all_by_complexity();
        for fmt in waterfall
            .into_iter()
            .rev()
            .cycle()
            .skip(5 - waterfall_index)
            .take_while(|&f| f != self.init_fmt)
        {
            if fmt.can_repr(IS_STRING, self.c) {
                self.fmt = fmt;
                match fmt {
                    CharFormat::Ascii => self.case.reset(2),
                    CharFormat::Unicode4 => self.case.reset(4),
                    CharFormat::Unicode6 => self.case.reset(6),
                    _ => (),
                };
                return true;
            }
        }
        false
    }

    fn update_buf(&mut self) {
        if self.fmt == CharFormat::Literal {
            self.c.encode_utf8(&mut self.buf);
        } else if self.fmt == CharFormat::SimpleEsc {
            self.buf[0] = b'\\';
            match self.c {
                '\n' => self.buf[1] = b'n',
                '\t' => self.buf[1] = b't',
                '\r' => self.buf[1] = b'r',
                '"' => self.buf[1] = b'"',
                '\'' => self.buf[1] = b'\'',
                _ => panic!("can't represent char with CharFormat::SimpleEsc"),
            }
        } else {
            let mut c_u32 = self.c as u32;
            for (i, c) in self.buf.iter_mut().enumerate().rev() {
                let val = (c_u32 & 0xf) as u8;
                *c = match val {
                    0..10 => val + b'0',
                    10..16 => {
                        val + if self.case.is_uppercase(i) {
                            b'A'
                        } else {
                            b'a'
                        } - 10
                    }
                    _ => unreachable!(),
                };
                c_u32 >>= 4;
            }
            match self.fmt {
                CharFormat::Literal | CharFormat::SimpleEsc => unreachable!(),
                CharFormat::Ascii => {
                    self.buf[4] = b'\\';
                    self.buf[5] = b'x';
                }
                CharFormat::Unicode4 => {
                    self.buf[2] = b'\\';
                    self.buf[3] = b'u';
                }
                CharFormat::Unicode6 => {
                    self.buf[0] = b'\\';
                    self.buf[1] = b'U';
                }
            };
        }
    }

    fn buf_range(&self) -> std::ops::Range<usize> {
        match self.fmt {
            CharFormat::Literal => 0..self.c.len_utf8(),
            CharFormat::Ascii => 4..8,
            CharFormat::Unicode4 => 2..8,
            CharFormat::Unicode6 => 0..8,
            CharFormat::SimpleEsc => 0..2,
        }
    }
    pub fn len(&self) -> usize {
        self.buf_range().len()
    }
    pub fn current_str(&self) -> &str {
        str::from_utf8(&self.buf[self.buf_range()])
            .expect("invalid utf8, this should be impossible")
    }
}

impl<const IS_STRING: bool> ValueTree for CharReprValueTree<IS_STRING> {
    type Value = String;

    fn current(&self) -> Self::Value {
        self.current_str().to_string()
    }

    fn simplify(&mut self) -> bool {
        if self.case_matters_for_format() && self.case.simplify() {
            self.update_buf();
            true
        } else if self.simplify_format() {
            self.update_buf();
            true
        } else {
            false
        }
    }

    fn complicate(&mut self) -> bool {
        if self.case_matters_for_format() && self.case.complicate() {
            self.update_buf();
            true
        } else if self.complicate_format() {
            self.update_buf();
            true
        } else {
            false
        }
    }
}

#[derive(Debug)]
pub struct CharReprStrategy<const IS_STRING: bool>(char);

impl CharReprStrategy<false> {
    pub fn new_for_char_literal(c: char) -> Self {
        Self(c)
    }
}

impl CharReprStrategy<true> {
    pub fn new_for_string_literal(c: char) -> Self {
        Self(c)
    }
}

impl<const IS_STRING: bool> Strategy for CharReprStrategy<IS_STRING> {
    type Tree = CharReprValueTree<IS_STRING>;

    type Value = String;

    fn new_tree(
        &self,
        runner: &mut proptest::test_runner::TestRunner,
    ) -> proptest::strategy::NewTree<Self> {
        use rand::seq::IteratorRandom;
        let fmt = *CharFormat::all_by_complexity()
            .iter()
            .filter(|c| c.can_repr(IS_STRING, self.0))
            .choose(runner.rng())
            .expect("cannot represent char in any format!");
        let case = CharFormatCase::new(
            runner.rng().gen_range(0..64),
            match fmt {
                CharFormat::Literal | CharFormat::Unicode6 | CharFormat::SimpleEsc => 6,
                CharFormat::Ascii => 2,
                CharFormat::Unicode4 => 4,
            },
        );
        let mut tree = Self::Tree {
            c: self.0,
            fmt,
            init_fmt: fmt,
            case,
            buf: [0; 8],
        };
        tree.update_buf();
        Ok(tree)
    }
}

pub struct StringReprValueTree {
    chars: Vec<CharReprValueTree<true>>,
    ind: usize,
    initial_ind: usize,
}

impl ValueTree for StringReprValueTree {
    type Value = String;

    fn current(&self) -> Self::Value {
        let mut buf = String::with_capacity(self.chars.iter().map(|c| c.len()).sum());
        for c in &self.chars {
            buf.push_str(c.current_str());
        }
        buf
    }

    fn simplify(&mut self) -> bool {
        while !self.chars[self.ind].simplify() {
            if (self.ind + 1) % self.chars.len() == self.initial_ind {
                return false;
            }
            if self.ind == self.chars.len() - 1 {
                self.ind = 0;
            } else {
                self.ind += 1;
            }
        }
        true
    }

    fn complicate(&mut self) -> bool {
        while !self.chars[self.ind].complicate() {
            if self.ind == (self.initial_ind + 1) % self.chars.len() {
                return false;
            }
            if self.ind == 0 {
                self.ind = self.chars.len() - 1
            } else {
                self.ind -= 1;
            }
        }
        true
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct StringReprStrategy(String);

impl Strategy for StringReprStrategy {
    type Tree = StringReprValueTree;
    type Value = String;

    fn new_tree(&self, runner: &mut proptest::test_runner::TestRunner) -> strategy::NewTree<Self> {
        let mut char_strategies = Vec::with_capacity(self.0.len());
        for c in self.0.chars() {
            char_strategies.push(CharReprStrategy::new_for_string_literal(c).new_tree(runner)?);
        }
        Ok(StringReprValueTree {
            chars: char_strategies,
            initial_ind: 0,
            ind: 0,
        })
    }
}

pub fn any_string_literal_content_char() -> impl Strategy<Value = (char, String)> {
    any::<char>().prop_flat_map(|c| (Just(c), CharReprStrategy::new_for_string_literal(c)))
}

pub fn any_string_literal_content() -> impl Strategy<Value = (String, String)> {
    any::<String>().prop_flat_map(|s| (Just(s.clone()), StringReprStrategy(s)))
}

pub fn any_char_literal_content() -> impl Strategy<Value = (char, String)> {
    any::<char>().prop_flat_map(|c| (Just(c), CharReprStrategy::new_for_char_literal(c)))
}
