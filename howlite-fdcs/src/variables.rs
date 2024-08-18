use std::{num::NonZeroUsize, sync::atomic::AtomicUsize, sync::atomic::Ordering};

use num_bigint::BigInt;
use thiserror::Error;

use crate::integer::{IntegerRange, IntegerSet};

#[derive(Clone, Debug)]
pub enum Mutation {
    Instantiate { value: BigInt },
    Exclude { value: BigInt },
    BoundLo { lo: BigInt },
    BoundHi { hi: BigInt },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Variable {
    Domain(IntegerSet),
    Instantiated(BigInt),
}

impl Variable {
    pub fn is_instantiated(&self) -> bool {
        matches!(self, &Self::Instantiated(_))
    }

    pub fn apply(self, mutation: Mutation) -> Result<Variable, InvalidMutationError> {
        match (self, mutation) {
            (Variable::Instantiated(_), _) => Err(InvalidMutationError::Instantiated),
            (Variable::Domain(domain), Mutation::Instantiate { value }) => {
                if domain.contains(&value) {
                    Ok(Variable::Instantiated(value))
                } else {
                    Err(InvalidMutationError::InstantiateOutOfDomain { value, domain })
                }
            }
            (Variable::Domain(mut domain), Mutation::Exclude { value }) => {
                if domain.exclude_value(&value) {
                    Ok(Variable::Domain(domain))
                } else {
                    Err(InvalidMutationError::ExcludeOutOfDomain { value, domain })
                }
            }
            (Variable::Domain(mut domain), Mutation::BoundLo { lo }) => {
                if let Some(range) = domain.range() {
                    if range.contains(&lo) {
                        domain.exclude_below(&lo);
                        Ok(Variable::Domain(domain))
                    } else {
                        Err(InvalidMutationError::LoOutOfRange { value: lo, range })
                    }
                } else {
                    Err(InvalidMutationError::BoundEmptySet)
                }
            }
            (Variable::Domain(mut domain), Mutation::BoundHi { hi }) => {
                if let Some(range) = domain.range() {
                    if range.contains(&hi) {
                        domain.exclude_above(&hi);
                        Ok(Variable::Domain(domain))
                    } else {
                        Err(InvalidMutationError::HiOutOfRange { value: hi, range })
                    }
                } else {
                    Err(InvalidMutationError::BoundEmptySet)
                }
            }
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct VariableId {
    id: NonZeroUsize,
}

impl VariableId {
    pub(crate) fn new() -> VariableId {
        static COUNTER: AtomicUsize = AtomicUsize::new(1);
        let counter = COUNTER.fetch_add(1, Ordering::Relaxed);
        let id = NonZeroUsize::new(counter)
            .expect("variable counter was zero, this should be uncreachable.");
        VariableId { id }
    }
}

#[derive(Debug, Clone, Error)]
pub enum InvalidMutationError {
    #[error("cannot mutate an instantiated variable")]
    Instantiated,

    #[error("cannot instantiate variable as {value}, domain is {domain:?}")]
    InstantiateOutOfDomain { value: BigInt, domain: IntegerSet },

    #[error("cannot exclude value {value}, not in domain {domain:?}")]
    ExcludeOutOfDomain { value: BigInt, domain: IntegerSet },

    #[error("cannot adjust bounds of an empty set")]
    BoundEmptySet,

    #[error("cannot adjust lower bound to expand domain: {value}, outside of range {range:?}")]
    LoOutOfRange { value: BigInt, range: IntegerRange },

    #[error("cannot adjust lower bound to expand domain: {value}, outside of range {range:?}")]
    HiOutOfRange { value: BigInt, range: IntegerRange },
}

impl std::fmt::Debug for VariableId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "var[{}]", self.id)
    }
}
