use std::{
    hash::{BuildHasher, DefaultHasher},
    num::{NonZeroU32, NonZeroUsize},
    sync::RwLock,
};

use hashbrown::{DefaultHashBuilder, HashTable};
use smol_str::SmolStr;

pub struct SyncSymbolTable<H: BuildHasher = DefaultHashBuilder> {
    inner: RwLock<OwnedSymbolTable<H>>,
}

impl Default for SyncSymbolTable {
    fn default() -> Self {
        Self {
            inner: RwLock::new(Default::default()),
        }
    }
}

impl SyncSymbolTable {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn stringify(&self, sym: Symbol) -> Result<SmolStr, SymbolTableError> {
        // TODO: recover from poison
        self.inner
            .read()
            .expect("SyncSymbolTable::stringify(): symbol table lock was poisoned!")
            .stringify(sym)
            .map(SmolStr::new)
    }

    pub fn intern(&self, s: &str) -> Symbol {
        // TODO: recover from poison
        self.inner
            .write()
            .expect("SyncSymbolTable::intern(): symbol table lock was poisoned!")
            .intern(s)
    }
}

pub struct OwnedSymbolTable<H: BuildHasher = DefaultHashBuilder> {
    alloc: bumpalo::Bump,

    // not actually, static, but for all intents and purposes, since they are self-referential
    table: SymbolTable<'static, H>,
}

impl Default for OwnedSymbolTable<DefaultHashBuilder> {
    fn default() -> Self {
        Self {
            alloc: Default::default(),
            table: Default::default(),
        }
    }
}

impl OwnedSymbolTable<DefaultHashBuilder> {
    pub fn new() -> Self {
        Self::default()
    }
}

impl<H: BuildHasher> OwnedSymbolTable<H> {
    pub fn new_with_hasher_builder(hasher_builder: H) -> Self {
        Self {
            alloc: bumpalo::Bump::new(),
            table: SymbolTable::new_with_hasher_builder(hasher_builder),
        }
    }

    pub fn stringify(&self, sym: Symbol) -> Result<&str, SymbolTableError> {
        self.table.stringify(sym)
    }

    pub fn intern(&mut self, s: &str) -> Symbol {
        let mem = self.alloc.alloc_str(s);
        let owned: &'static mut str = unsafe { std::mem::transmute(mem) };
        self.table.intern(owned)
    }
}

// implementation note: symbols are offset + length into a string table packed into a u64
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Symbol {
    /// reserve zero for enum variants
    seq: NonZeroUsize,
} // store the hash and sequence number of the symbol

impl std::fmt::Debug for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<Symbol {:016x}>", self.seq)
    }
}

pub struct SymbolTable<'a, H: BuildHasher = DefaultHashBuilder> {
    hasher_builder: H,
    strings: Vec<&'a str>,
    symbols: HashTable<Symbol>,
}

impl<'a> Default for SymbolTable<'a, DefaultHashBuilder> {
    fn default() -> Self {
        Self::new_with_hasher_builder(Default::default())
    }
}

impl<'a> SymbolTable<'a, DefaultHashBuilder> {
    pub fn new() -> Self {
        Self::default()
    }
}

impl<'a, H: BuildHasher> SymbolTable<'a, H> {
    pub fn new_with_hasher_builder(hasher_builder: H) -> Self {
        Self {
            hasher_builder,
            symbols: Default::default(),
            strings: Vec::new(),
        }
    }

    pub fn stringify(&self, sym: Symbol) -> Result<&'a str, SymbolTableError> {
        self.strings
            .get(sym.seq.get() - 1)
            .map(|s| *s)
            .ok_or(SymbolTableError::InvalidSymbol { symbol: sym })
    }

    pub fn intern(&mut self, s: &'a str) -> Symbol {
        let hash = self.hasher_builder.hash_one(s);

        let existing = self
            .symbols
            .find(hash, |sym| self.strings[sym.seq.get() - 1] == s);
        if let Some(sym) = existing {
            *sym
        } else {
            self.strings.push(s);
            let seq = NonZeroUsize::new(self.strings.len()).expect(
                "Array of strings for symbol table had a length < 1 after inserting an element",
            );
            let sym = Symbol { seq };
            self.symbols.insert_unique(hash, sym, |s| {
                self.hasher_builder.hash_one(self.strings[s.seq.get() - 1])
            });
            sym
        }
    }
}

#[derive(thiserror::Error, Debug)]
pub enum SymbolTableError {
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

    use crate::symtab::{OwnedSymbolTable, SymbolTable};

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
            let s = strgen.random_str().to_string().leak();
            symtab.intern(s);
        }
    }

    #[test]
    fn owned_intern() {
        let mut strgen: RandomStringGen<10, 1000, _> =
            RandomStringGen::new(StdRng::seed_from_u64(13));

        println!("adding 1k symbols...");

        let mut symtab = OwnedSymbolTable::new();
        let mut symbols = Vec::with_capacity(1000);
        let mut symbol_strings = Vec::with_capacity(1000);
        for _ in 0..1000 {
            let s = strgen.random_str();
            symbol_strings.push(s.to_string());
            symbols.push(symtab.intern(s));
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
            symbol_strings.push(s.to_string());
            symbols.push(symtab.intern(s.to_string().leak()));
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
