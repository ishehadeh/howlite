pub mod environment;
pub mod integer;

pub use variables::{InvalidMutationError, Mutation};

pub mod variables;
pub use integer::{IntegerRange, IntegerSet};

pub mod constraints;

#[cfg(test)]
mod test;
