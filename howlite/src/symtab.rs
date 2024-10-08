use std::{
    hash::BuildHasher,
    sync::atomic::{AtomicU64, Ordering},
};

use hashbrown::{hash_map::DefaultHashBuilder, HashTable};

// implementation note: symbols are offset + length into a string table packed into a u64
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Symbol(u64, u64); // store the hash and sequence number of the symbol

impl std::fmt::Debug for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<Symbol {:016x}.{:016x}>", self.0, self.1)
    }
}

pub struct SymbolTable<'a, H: BuildHasher = DefaultHashBuilder> {
    hasher_builder: H,
    sequence: AtomicU64,
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
    fn new() -> Self {
        Self::default()
    }
}

impl<'a, H: BuildHasher> SymbolTable<'a, H> {
    pub fn new_with_hasher_builder(hasher_builder: H) -> Self {
        Self {
            hasher_builder,
            sequence: AtomicU64::new(0),
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

    pub fn intern(&mut self, s: &'a str) -> Result<Symbol, SymbolTableError> {
        // this is probably the most naive symbol table you could implement, but it works well enough...
        let s_hash = self.hasher_builder.hash_one(s);
        let existing = self
            .symbols
            .find(s_hash, |(t_hash, _, t)| *t_hash == s_hash && *t == s);
        if let Some((_, s_seq, _)) = existing {
            Ok(Symbol(s_hash, *s_seq))
        } else {
            let s_seq = self.sequence.fetch_add(1, Ordering::Relaxed);
            self.symbols
                .insert_unique(s_hash, (s_hash, s_seq, s), |(_, _, t)| {
                    self.hasher_builder.hash_one(t)
                });
            Ok(Symbol(s_hash, s_seq))
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
            let s = strgen.random_str().to_string().leak();
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
            symbol_strings.push(s.to_string());
            symbols.push(
                symtab
                    .intern(s.to_string().leak())
                    .expect("failed to intern string"),
            );
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