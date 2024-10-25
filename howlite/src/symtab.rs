use std::{hash::BuildHasher, sync::RwLock};

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
pub struct Symbol(u64, u64); // store the hash and sequence number of the symbol

impl std::fmt::Debug for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<Symbol {:016x}.{:016x}>", self.0, self.1)
    }
}

pub struct SymbolTable<'a, H: BuildHasher = DefaultHashBuilder> {
    hasher_builder: H,
    sequence: u64,
    symbols: HashTable<(u64, u64, &'a str)>,
}

impl<'a> Default for SymbolTable<'a, DefaultHashBuilder> {
    fn default() -> Self {
        Self {
            hasher_builder: Default::default(),
            sequence: Default::default(),
            symbols: Default::default(),
        }
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
            sequence: 0,
            symbols: Default::default(),
        }
    }

    pub fn stringify(&self, sym: Symbol) -> Result<&'a str, SymbolTableError> {
        let Symbol(hash, seq) = sym;
        self.symbols
            .find(hash, move |(b_hash, b_seq, _)| {
                *b_hash == hash && *b_seq == seq
            })
            .map(|(_, _, s)| *s)
            .ok_or(SymbolTableError::InvalidSymbol { symbol: sym })
    }

    pub fn intern(&mut self, s: &'a str) -> Symbol {
        let s_hash = self.hasher_builder.hash_one(s);
        let existing = self
            .symbols
            .find(s_hash, |(t_hash, _, t)| *t_hash == s_hash && *t == s);
        if let Some((_, s_seq, _)) = existing {
            Symbol(s_hash, *s_seq)
        } else {
            let s_seq = self.sequence;
            self.sequence += 1;
            self.symbols
                .insert_unique(s_hash, (s_hash, s_seq, s), |(_, _, t)| {
                    self.hasher_builder.hash_one(t)
                });
            Symbol(s_hash, s_seq)
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
