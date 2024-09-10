#[derive(thiserror::Error, miette::Diagnostic, Debug)]
pub enum AccessError {
    #[error("cannot access field on non-struct type")]
    IllegalFieldAccess,

    #[error("cannot access fields on unions which contain a non-struct type")]
    NonStructUnionVariant,

    #[error("cannot access index on non-slice, non-array type")]
    IllegalIndexAccess,
}
