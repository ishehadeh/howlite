mod errors;
pub use errors::{CompilationError, CompilationErrorKind};

pub mod langctx;
pub mod symtab;
pub mod typetree;

pub fn add(left: u64, right: u64) -> u64 {
    left + right
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let result = add(2, 2);
        assert_eq!(result, 4);
    }
}
