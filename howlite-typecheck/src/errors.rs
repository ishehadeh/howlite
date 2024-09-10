#[derive(thiserror::Error, miette::Diagnostic, Debug)]
pub enum AccessError {
    #[error("cannot access field on non-struct type")]
    IllegalFieldAccess,

    #[error("cannot access fields on unions which contain a non-struct type")]
    NonStructUnionVariant,

    #[error("All union variants must contain a field to access it")]
    FieldMissingInUnionVariant,

    #[error("Although the field exists on all union variants, it cannot be accessed since it is at different offsets.")]
    FieldMisaligned,

    #[error("cannot access index on non-slice, non-array type")]
    IllegalIndexAccess,
}
