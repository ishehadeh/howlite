use num_bigint::BigInt;

use crate::{integer::IntegerRange, Constraint, ConstraintContext, Event, Mutation, Variable};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Comparison {
    Less,    // reused for greater/equal
    Greater, // reused for less/equal
}

#[derive(Clone, Debug)]
pub struct OffsetCompare {
    pub lhs: Variable,
    pub lhs_offset: BigInt,
    pub rhs: Variable,
    pub op: Comparison,
}

impl OffsetCompare {
    pub fn less(lhs: Variable, offset: impl Into<BigInt>, rhs: Variable) -> OffsetCompare {
        OffsetCompare {
            lhs,
            lhs_offset: offset.into(),
            rhs,
            op: Comparison::Less,
        }
    }

    pub fn greater(lhs: Variable, offset: impl Into<BigInt>, rhs: Variable) -> OffsetCompare {
        OffsetCompare {
            lhs,
            lhs_offset: offset.into(),
            rhs,
            op: Comparison::Less,
        }
    }

    // propogate when the lhs var is mutated
    fn propogate_lhs(&self, mut ctx: ConstraintContext, event: Event) {
        match event.mutation {
            crate::Mutation::Instantiate { binding } => {
                todo!("impl binding")
            }
            crate::Mutation::Bound { range } => {
                let offset_range = range.offset(self.lhs_offset.clone());
                let rhs_range = ctx.variables.get(self.rhs).range().unwrap();
                match self.op {
                    // no update needed
                    Comparison::Less if offset_range.hi < rhs_range.lo => (),

                    // constraint failed
                    Comparison::Less if offset_range.lo >= rhs_range.lo => {
                        todo!("constraint failed")
                    }

                    // update to rhs needed.
                    Comparison::Less if offset_range.hi >= rhs_range.lo => {
                        ctx.submit(
                            self.rhs,
                            Mutation::Bound {
                                range: ctx
                                    .variables
                                    .get(self.lhs)
                                    .range()
                                    .unwrap()
                                    .with_lo(offset_range.lo + 1),
                            },
                        );
                    }
                    _ => todo!(),
                }
            }
            crate::Mutation::Exclude { excluded } => todo!(),
        }
    }

    // propogate when the rhs var is mutated
    fn propogate_rhs(&self, mut ctx: ConstraintContext, event: Event) {
        match event.mutation {
            crate::Mutation::Instantiate { binding } => {
                todo!("impl binding")
            }
            crate::Mutation::Bound { range } => {
                let offset_range = ctx
                    .variables
                    .get(self.lhs)
                    .range()
                    .unwrap()
                    .offset(self.lhs_offset.clone());
                let rhs_range = range;
                match self.op {
                    // no update needed
                    Comparison::Less if offset_range.hi < rhs_range.lo => (),

                    // constraint failed
                    Comparison::Less if offset_range.lo >= rhs_range.lo => {
                        todo!("constraint failed")
                    }

                    // update to lhs needed.
                    Comparison::Less if offset_range.hi >= rhs_range.lo => {
                        ctx.submit(
                            self.lhs,
                            Mutation::Bound {
                                range: ctx
                                    .variables
                                    .get(self.lhs)
                                    .range()
                                    .unwrap()
                                    .with_hi(rhs_range.hi - 1 - self.lhs_offset.clone()),
                            },
                        );
                    }
                    _ => todo!(),
                }
            }
            crate::Mutation::Exclude { excluded } => todo!(),
        }
    }
}

impl Constraint for OffsetCompare {
    fn propogate(&mut self, mut ctx: ConstraintContext, event: Event) {
        if event.subject == self.lhs {
            self.propogate_lhs(ctx, event)
        } else if event.subject == self.rhs {
            self.propogate_rhs(ctx, event)
        }
    }

    fn initialize(&mut self, mut ctx: ConstraintContext) {
        let rhs_range = ctx.variables.get(self.rhs).range().unwrap();
        let lhs_range = ctx
            .variables
            .get(self.lhs)
            .range()
            .unwrap()
            .offset(self.lhs_offset.clone());
        ctx.submit(
            self.lhs,
            Mutation::Bound {
                range: lhs_range
                    .clone()
                    .with_hi(rhs_range.lo.clone() - 1 - self.lhs_offset.clone()),
            },
        );
        ctx.submit(
            self.rhs,
            Mutation::Bound {
                range: rhs_range.with_lo(lhs_range.hi + 1 + self.lhs_offset.clone()),
            },
        );
    }
}
