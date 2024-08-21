pub mod environment;
pub mod integer;
// pub mod environment2;

pub use variables::{InvalidMutationError, Mutation};

pub mod variables;
pub use integer::{IntegerRange, IntegerSet};

pub mod constraints;

#[cfg(test)]
mod test;
