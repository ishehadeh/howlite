use num_traits::One;
use sunstone::ops::Bounded;

use crate::{
    environment::{Constraint, Event, NarrowResult, PropogationEnvironment},
    integer::num_bigint::BigInt,
    variables::{Mutation, VariableId},
};

#[derive(Clone, Debug)]
pub struct OffsetLtConstraint {
    pub lhs: VariableId,
    pub lhs_offset: BigInt,
    pub rhs: VariableId,
}

impl OffsetLtConstraint {
    pub fn lt_offset(
        lhs: VariableId,
        offset: impl Into<BigInt>,
        rhs: VariableId,
    ) -> OffsetLtConstraint {
        OffsetLtConstraint {
            lhs,
            lhs_offset: offset.into(),
            rhs,
        }
    }

    pub fn gt_offset(
        lhs: VariableId,
        offset: impl Into<BigInt>,
        rhs: VariableId,
    ) -> OffsetLtConstraint {
        OffsetLtConstraint {
            lhs: rhs,
            lhs_offset: -offset.into(),
            rhs: lhs,
        }
    }

    pub fn lt_eq_offset(
        lhs: VariableId,
        offset: impl Into<BigInt>,
        rhs: VariableId,
    ) -> OffsetLtConstraint {
        Self::lt_offset(lhs, offset.into() + 1, rhs)
    }

    pub fn gt_eq_offset(
        lhs: VariableId,
        offset: impl Into<BigInt>,
        rhs: VariableId,
    ) -> OffsetLtConstraint {
        Self::gt_offset(lhs, offset.into() - 1, rhs)
    }

    pub fn gt_eq(lhs: VariableId, rhs: VariableId) -> OffsetLtConstraint {
        Self::gt_eq_offset(lhs, BigInt::ZERO, rhs)
    }

    pub fn lt_eq(lhs: VariableId, rhs: VariableId) -> OffsetLtConstraint {
        Self::lt_eq_offset(lhs, BigInt::ZERO, rhs)
    }

    pub fn gt(lhs: VariableId, rhs: VariableId) -> OffsetLtConstraint {
        Self::gt_offset(lhs, BigInt::ZERO, rhs)
    }

    pub fn lt(lhs: VariableId, rhs: VariableId) -> OffsetLtConstraint {
        Self::lt_offset(lhs, BigInt::ZERO, rhs)
    }

    /// Try to constrain the right hand side in order to satisfy the constraint.
    /// If the constraint was not fully satisfied, return the adjustment needed on the left hand side.
    fn constrain_rhs(
        &self,
        ctx: &mut PropogationEnvironment,
        adjustment: Option<BigInt>,
    ) -> NarrowResult {
        let rhs_range = ctx.variable_range(self.rhs).unwrap();
        let lhs_range = ctx.variable_range(self.lhs).unwrap();

        let adjustment_needed = adjustment.unwrap_or(
            (lhs_range.hi().clone() + &self.lhs_offset + BigInt::one()) - rhs_range.lo(),
        );
        if adjustment_needed <= BigInt::ZERO {
            // no adjustment needed if rhs lo > lhs hi
            return NarrowResult::Satisfied;
        }
        dbg!(&adjustment_needed);

        let allowed_rhs_lo_adjustment = rhs_range.hi().clone() - rhs_range.lo();
        assert!(allowed_rhs_lo_adjustment >= BigInt::ZERO);

        if allowed_rhs_lo_adjustment == BigInt::ZERO {
            NarrowResult::Violation
        } else {
            NarrowResult::Narrow(
                self.rhs,
                Mutation::BoundLo {
                    lo: rhs_range.lo().clone() + adjustment_needed.min(allowed_rhs_lo_adjustment),
                },
            )
        }
    }

    fn constrain_lhs(
        &self,
        ctx: &mut PropogationEnvironment,
        adjustment: Option<BigInt>,
    ) -> NarrowResult {
        // TODO: merge at least part of this with constraint rhs
        let rhs_range = ctx.variable_range(self.rhs).unwrap();
        let lhs_range = ctx.variable_range(self.lhs).unwrap();

        let adjustment_needed =
            adjustment.unwrap_or((lhs_range.hi().clone() + &self.lhs_offset + 1) - rhs_range.lo());
        if adjustment_needed <= BigInt::ZERO {
            // no adjustment needed if rhs lo > lhs hi
            return NarrowResult::Satisfied;
        }

        let allowed_lhs_hi_adjustment = lhs_range.hi() - lhs_range.lo();
        assert!(allowed_lhs_hi_adjustment >= BigInt::ZERO);
        if allowed_lhs_hi_adjustment == BigInt::ZERO {
            NarrowResult::Violation
        } else {
            NarrowResult::Narrow(
                self.lhs,
                Mutation::BoundHi {
                    hi: lhs_range.hi() - adjustment_needed.min(allowed_lhs_hi_adjustment),
                },
            )
        }
    }
}

impl Constraint for OffsetLtConstraint {
    fn propogate(&self, ctx: &mut PropogationEnvironment, event: Option<&Event>) -> NarrowResult {
        match event {
            Some(event) if event.variable == self.lhs => {
                let rhs_result = self.constrain_rhs(ctx, None);
                if matches!(rhs_result, NarrowResult::Violation) {
                    self.constrain_lhs(ctx, None)
                } else {
                    rhs_result
                }
            }
            Some(_) | None => {
                let lhs_result = self.constrain_lhs(ctx, None);
                if matches!(lhs_result, NarrowResult::Violation) {
                    self.constrain_rhs(ctx, None)
                } else {
                    lhs_result
                }
            }
        }
    }
}
