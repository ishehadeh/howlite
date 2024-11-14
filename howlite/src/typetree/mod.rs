//! Type Tree
//! -----------
//!
//! This module maps AST nodes to types.
//!
//! Depending on the node, the associated type may have different meanings.
//! For example, ast::Ty[...] nodes are mapped to the actual type they represent.
//! On the other hand, most expressions are mapped to the type representing the set of possible values
//! that would result from the given expression.

mod constraint_term;
mod constraint_tree;
mod infix;
mod original;
mod traits;
mod ty;

// pub use infix::*;

pub use constraint_term::*;
// pub use constraint_tree::*;

#[cfg(test)]
mod test_helpers;

// pub use original::*;
pub use traits::{SynthesizeTy, SynthesizeTyPure};
