use num_bigint::BigInt;

use crate::{
    environment::{Constraint, PropogationEnvironment},
    variables::{Mutation, VariableId},
};
mod add;

pub use add::BinaryAddConstraint;

#[derive(Clone, Debug)]
pub struct OffsetLtConstraint {
    pub lhs: VariableId,
    pub lhs_offset: BigInt,
    pub rhs: VariableId,
}

impl OffsetLtConstraint {
    pub fn new(lhs: VariableId, offset: impl Into<BigInt>, rhs: VariableId) -> OffsetLtConstraint {
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
        ctx: &mut PropogationEnvironment,
        adjustment: Option<BigInt>,
    ) -> Option<BigInt> {
        let rhs_range = ctx.variable_range(self.rhs).unwrap();
        let lhs_range = ctx.variable_range(self.lhs).unwrap();

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
            ctx.mutate(self.rhs, Mutation::BoundLo { lo: rhs_range.lo })
                .unwrap();
            Some(adjustment_needed - allowed_rhs_lo_adjustment)
        } else {
            ctx.mutate(
                self.rhs,
                Mutation::BoundLo {
                    lo: rhs_range.lo + adjustment_needed,
                },
            )
            .unwrap();
            None
        }
    }

    fn constrain_lhs(
        &self,
        ctx: &mut PropogationEnvironment,
        adjustment: Option<BigInt>,
    ) -> Option<BigInt> {
        // TODO: merge at least part of this with constraint rhs
        let rhs_range = ctx.variable_range(self.rhs).unwrap();
        let lhs_range = ctx.variable_range(self.lhs).unwrap();

        let adjustment_needed =
            adjustment.unwrap_or((&lhs_range.hi + &self.lhs_offset + 1) - &rhs_range.lo);
        if adjustment_needed <= BigInt::ZERO {
            // no adjustment needed if rhs lo > lhs hi
            return None;
        }

        let allowed_lhs_hi_adjustment = &lhs_range.hi - &lhs_range.lo;
        assert!(allowed_lhs_hi_adjustment >= BigInt::ZERO);
        if allowed_lhs_hi_adjustment < adjustment_needed {
            ctx.mutate(self.lhs, Mutation::BoundHi { hi: lhs_range.hi })
                .unwrap();

            Some(adjustment_needed - allowed_lhs_hi_adjustment)
        } else {
            ctx.mutate(
                self.lhs,
                Mutation::BoundHi {
                    hi: lhs_range.hi - adjustment_needed,
                },
            )
            .unwrap();
            None
        }
    }
}

impl Constraint for OffsetLtConstraint {
    fn propogate(&mut self, ctx: &mut PropogationEnvironment) -> bool {
        if let Some(lhs_rem) = self.constrain_lhs(ctx, None) {
            self.constrain_rhs(ctx, Some(lhs_rem)).is_some()
        } else {
            true
        }
    }
}
