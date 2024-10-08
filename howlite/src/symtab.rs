// implementation note: symbols are offset + length into a string table packed into a u64
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Symbol(u64);

impl std::fmt::Debug for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<Symbol {:016x}>", self.0)
    }
}

#[derive(Default)]
pub struct SymbolTable {
    symbols: String,
}

/// Maximum length of an individual symbol
const SYMBOL_STRING_MAX_LEN: usize = u16::MAX as usize;

/// Total length of all strings in the table
const SYMBOL_TABLE_MAX_LEN: usize = {
    #[cfg(target_endian = "little")]
    let max_len = u64::MAX >> (u64::BITS - SYMBOL_STRING_MAX_LEN.leading_zeros());

    #[cfg(target_endian = "big")]
    let max_len = u64::MAX << (u64::BITS - SYMBOL_STRING_MAX_LEN.trailing_zeros());

    let max_string_len = usize::MAX as u64;
    if max_len < max_string_len {
        max_len as usize
    } else {
        max_string_len as usize
    }
};

// use least significant bits for length, so ordering compares index first.
#[cfg(target_endian = "little")]
const SYMBOL_LEN_BIT_OFFSET: usize = 0;

#[cfg(target_endian = "little")]
const SYMBOL_INDEX_BIT_OFFSET: usize =
    u64::BITS as usize - SYMBOL_STRING_MAX_LEN.leading_zeros() as usize;

#[cfg(target_endian = "big")]
const SYMBOL_LEN_BIT_OFFET: usize = SYMBOL_STRING_MAX_LEN.trailing_zeros();

#[cfg(target_endian = "big")]
const SYMBOL_INDEX_BIT_OFFSET: usize = 0;

#[inline(always)]
fn encode_symbol_ref(index: usize, len: usize) -> u64 {
    (((index & SYMBOL_TABLE_MAX_LEN) as u64) << SYMBOL_INDEX_BIT_OFFSET)
        | (((len & SYMBOL_STRING_MAX_LEN) as u64) << SYMBOL_LEN_BIT_OFFSET)
}

#[inline(always)]
const fn decode_symbol_ref(sym_ref: u64) -> (usize, usize) {
    (
        ((sym_ref >> SYMBOL_INDEX_BIT_OFFSET) & SYMBOL_TABLE_MAX_LEN as u64) as usize, // index into symbol table
        ((sym_ref >> SYMBOL_LEN_BIT_OFFSET) & SYMBOL_STRING_MAX_LEN as u64) as usize,  // length
    )
}

impl SymbolTable {
    pub fn new() -> SymbolTable {
        SymbolTable {
            symbols: String::new(),
        }
    }

    pub fn stringify(&mut self, sym: Symbol) -> Result<&str, SymbolTableError> {
        let (ind, len) = decode_symbol_ref(sym.0);
        if self.symbols.len() < ind + len {
            Err(SymbolTableError::InvalidSymbol { symbol: sym })
        } else {
            Ok(&self.symbols[ind..ind + len])
        }
    }

    pub fn intern(&mut self, s: &str) -> Result<Symbol, SymbolTableError> {
        if s.len() > SYMBOL_STRING_MAX_LEN {
            Err(SymbolTableError::StringTooLong { length: s.len() })
        } else {
            let index = self.symbols.len();
            if SYMBOL_TABLE_MAX_LEN - s.len() < index {
                Err(SymbolTableError::SymbolTableCapacityExceeded {
                    symbol: s.to_owned(),
                })
            } else {
                self.symbols.push_str(s);

                Ok(Symbol(encode_symbol_ref(index, s.len())))
            }
        }
    }
}

#[derive(thiserror::Error, Debug)]
pub enum SymbolTableError {
    #[error(
        "Cannot intern symbols which exceed {} bytes (got: {} bytes), when encoded as UTF-8.",
        SYMBOL_STRING_MAX_LEN,
        length
    )]
    StringTooLong { length: usize },

    #[error(
        "Cannot intern symbol: {}, maximum symbol table capacity ({} bytes) would be exceeded.",
        symbol,
        SYMBOL_TABLE_MAX_LEN
    )]
    SymbolTableCapacityExceeded { symbol: String },

    #[error(
        "Cannot retrieve string for symbol {:?}, it may not belong to this table",
        symbol
    )]
    InvalidSymbol { symbol: Symbol },
}

#[cfg(test)]
mod test {
    use core::str;

    use rand::{rngs::StdRng, SeedableRng};

    use crate::symtab::SymbolTable;

    struct RandomStringGen<const MIN_LEN: usize, const MAX_LEN: usize, R: rand::Rng> {
        rng: R,
        buffer: [u8; MAX_LEN],
    }

    impl<const MIN_LEN: usize, const MAX_LEN: usize, R: rand::Rng>
        RandomStringGen<MIN_LEN, MAX_LEN, R>
    {
        pub fn new(rng: R) -> Self {
            Self {
                rng,
                buffer: [0; MAX_LEN],
            }
        }

        pub fn random_str(&mut self) -> &str {
            const ALPHABET: &[u8] =
                b"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789";

            let strlen = self.rng.gen_range(MIN_LEN..MAX_LEN);
            for buf_c in self.buffer.iter_mut() {
                let random_chr = ALPHABET[self.rng.gen_range(0..ALPHABET.len())];
                *buf_c = random_chr;
            }

            str::from_utf8(&self.buffer[0..strlen])
                .expect("non- UTF-8 random string generated, this should be impossible")
        }
    }

    #[test]
    fn add_symbols_large() {
        let mut strgen: RandomStringGen<10, 1000, _> =
            RandomStringGen::new(StdRng::seed_from_u64(42));

        println!("adding 1k symbols...");

        let mut symtab = SymbolTable::new();
        for _ in 0..1000 {
            let s = strgen.random_str();
            symtab.intern(s).expect("failed to intern string");
        }
    }

    #[test]
    fn add_and_retrieve_symbols_large() {
        let mut strgen: RandomStringGen<10, 1000, _> =
            RandomStringGen::new(StdRng::seed_from_u64(13));

        println!("adding 1k symbols...");

        let mut symtab = SymbolTable::new();
        let mut symbols = Vec::with_capacity(1000);
        let mut symbol_strings = Vec::with_capacity(1000);
        for _ in 0..1000 {
            let s = strgen.random_str();
            symbols.push(symtab.intern(s).expect("failed to intern string"));
            symbol_strings.push(s.to_string());
        }

        for (i, &sym) in symbols.iter().enumerate() {
            assert_eq!(
                symtab.stringify(sym).expect("failed to retrieve symbol"),
                symbol_strings[i],
                "failed on symbol {} ({:?})",
                i,
                sym
            );
        }
    }
}
