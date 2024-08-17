use num_bigint::BigInt;

use crate::{integer::IntegerRange, Constraint, ConstraintContext, Event, Mutation, Variable};
mod add;

pub use add::BinaryAddConstraint;

#[derive(Clone, Debug)]
pub struct OffsetLtConstraint {
    pub lhs: Variable,
    pub lhs_offset: BigInt,
    pub rhs: Variable,
}

impl OffsetLtConstraint {
    pub fn new(lhs: Variable, offset: impl Into<BigInt>, rhs: Variable) -> OffsetLtConstraint {
        OffsetLtConstraint {
            lhs,
            lhs_offset: offset.into(),
            rhs,
        }
    }

    /// Try to constrain the right hand side in order to satisfy the constraint.
    /// If the constraint was not fully satisfied, return the adjustment needed on the left hand side.
    fn constrain_rhs(
        &self,
        ctx: &mut ConstraintContext,
        adjustment: Option<BigInt>,
    ) -> Option<BigInt> {
        let rhs_range = ctx.variables.get(self.rhs).range().unwrap();
        let lhs_range = ctx.variables.get(self.lhs).range().unwrap();

        let adjustment_needed =
            adjustment.unwrap_or((&lhs_range.hi + &self.lhs_offset + 1) - (&rhs_range.lo));
        if adjustment_needed <= BigInt::ZERO {
            // no adjustment needed if rhs lo > lhs hi
            return None;
        }
        dbg!(&adjustment_needed);

        let allowed_rhs_lo_adjustment = &rhs_range.hi - &rhs_range.lo;
        assert!(allowed_rhs_lo_adjustment >= BigInt::ZERO);
        if allowed_rhs_lo_adjustment < adjustment_needed {
            ctx.submit(
                self.rhs,
                Mutation::Bound {
                    range: IntegerRange::new(rhs_range.hi.clone(), rhs_range.hi),
                },
            );

            Some(adjustment_needed - allowed_rhs_lo_adjustment)
        } else {
            ctx.submit(
                self.rhs,
                Mutation::Bound {
                    range: IntegerRange::new(rhs_range.lo + adjustment_needed, rhs_range.hi),
                },
            );
            None
        }
    }

    fn constrain_lhs(
        &self,
        ctx: &mut ConstraintContext,
        adjustment: Option<BigInt>,
    ) -> Option<BigInt> {
        // TODO: merge at least part of this with constraint rhs
        let rhs_range = ctx.variables.get(self.rhs).range().unwrap();
        let lhs_range = ctx.variables.get(self.lhs).range().unwrap();

        let adjustment_needed =
            adjustment.unwrap_or((&lhs_range.hi + &self.lhs_offset + 1) - &rhs_range.lo);
        if adjustment_needed <= BigInt::ZERO {
            // no adjustment needed if rhs lo > lhs hi
            return None;
        }

        let allowed_lhs_hi_adjustment = &lhs_range.hi - &lhs_range.lo;
        assert!(allowed_lhs_hi_adjustment >= BigInt::ZERO);
        if allowed_lhs_hi_adjustment < adjustment_needed {
            ctx.submit(
                self.lhs,
                Mutation::Bound {
                    range: IntegerRange::new(lhs_range.lo.clone(), lhs_range.lo),
                },
            );

            Some(adjustment_needed - allowed_lhs_hi_adjustment)
        } else {
            ctx.submit(
                self.lhs,
                Mutation::Bound {
                    range: IntegerRange::new(lhs_range.lo, lhs_range.hi - adjustment_needed),
                },
            );
            None
        }
    }
}

impl Constraint for OffsetLtConstraint {
    fn propogate(&mut self, mut ctx: ConstraintContext, event: Event) {
        if event.subject == self.rhs {
            if let Some(lhs_rem) = self.constrain_lhs(&mut ctx, None) {
                if self.constrain_rhs(&mut ctx, Some(lhs_rem)).is_some() {
                    panic!("constraint failed")
                }
            }
        } else if event.subject == self.lhs {
            if let Some(rhs_rem) = self.constrain_rhs(&mut ctx, None) {
                if self.constrain_lhs(&mut ctx, Some(rhs_rem)).is_some() {
                    panic!("constraint failed")
                }
            }
        }
    }

    fn initialize(&mut self, mut ctx: ConstraintContext) {
        if let Some(rhs_rem) = self.constrain_rhs(&mut ctx, None) {
            if self.constrain_lhs(&mut ctx, Some(rhs_rem)).is_some() {
                panic!("constraint failed")
            }
        }
    }
}
