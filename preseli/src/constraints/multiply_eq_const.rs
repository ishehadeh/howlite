use crate::{
    environment::{Constraint, Event, NarrowResult, PropogationEnvironment},
    integer::{shift_hi_mutation, shift_lo_mutation, Scalar},
    variables::{Mutation, Variable, VariableId},
};
use num_integer::Integer;
use sunstone::ops::{ArithmeticSet, Bounded, SetSubtract};

#[derive(Clone, Debug)]
pub struct MultiplyConstEqConstraint {
    pub lhs: VariableId,
    pub lhs_coefficient: Scalar,
    pub rhs: VariableId,
}

impl MultiplyConstEqConstraint {
    pub fn new(
        lhs: VariableId,
        lhs_coefficient: Scalar,
        rhs: VariableId,
    ) -> MultiplyConstEqConstraint {
        MultiplyConstEqConstraint {
            lhs,
            lhs_coefficient,
            rhs,
        }
    }
    fn narrow_lhs(&self, ctx: &mut PropogationEnvironment) -> NarrowResult {
        let (is_mutable, lhs_var) = match ctx.variable(self.lhs) {
            Variable::Instantiated(d) => (false, d.clone()),
            Variable::Domain(d) => (true, d.clone()),
        };

        let mut lhs = lhs_var.clone();
        lhs.mul_scalar(&self.lhs_coefficient);
        let rhs = match ctx.variable(self.rhs) {
            Variable::Instantiated(x) => x,
            Variable::Domain(d) => d,
        };

        if lhs.is_empty() || rhs.is_empty() {
            return NarrowResult::Violation;
        }

        let lhs_range = lhs.get_range();
        // let lhs_var_range = lhs_var.get_range();
        let rhs_range = rhs.get_range();
        if lhs_range != rhs_range && !is_mutable {
            return NarrowResult::Violation;
        }
        // dbg!(&lhs_range);

        if lhs_range.hi() > rhs_range.hi() {
            return shift_hi_mutation(
                &lhs_var,
                (lhs_range.hi() - rhs_range.hi()).div_ceil(&self.lhs_coefficient),
            )
            .map(|m| NarrowResult::Narrow(self.lhs, m))
            .unwrap_or(NarrowResult::Violation);
        }
        if lhs_range.lo() < rhs_range.lo() {
            return shift_lo_mutation(
                &lhs_var,
                (rhs_range.lo() - lhs_range.lo()).div_ceil(&self.lhs_coefficient),
            )
            .map(|m| NarrowResult::Narrow(self.lhs, m))
            .unwrap_or(NarrowResult::Violation);
        }

        let mut lhs_only = lhs.clone();
        lhs_only.set_subtract_mut(rhs);
        dbg!(&lhs_only);
        if lhs_only.is_empty() {
            NarrowResult::Satisfied
        } else if lhs_only.is_divisible_by(&self.lhs_coefficient) {
            lhs_only.div_scalar(&self.lhs_coefficient);
            NarrowResult::Narrow(self.lhs, Mutation::Exclude { values: lhs_only })
        } else {
            self.narrow_rhs(ctx)
        }
    }

    fn narrow_rhs(&self, ctx: &mut PropogationEnvironment) -> NarrowResult {
        let (is_mutable, rhs) = match ctx.variable(self.rhs) {
            Variable::Instantiated(d) => (false, d.clone()),
            Variable::Domain(d) => (true, d.clone()),
        };

        let lhs = match ctx.variable(self.lhs) {
            Variable::Instantiated(x) => x.clone(),
            Variable::Domain(d_ref) => {
                let mut d = d_ref.clone();
                d.mul_scalar(&self.lhs_coefficient);
                d
            }
        };

        if lhs.is_empty() || rhs.is_empty() {
            return NarrowResult::Violation;
        }

        let lhs_range = lhs.get_range();
        let rhs_range = rhs.get_range();
        if lhs_range != rhs_range && !is_mutable {
            return NarrowResult::Violation;
        }

        if rhs_range.hi() > lhs_range.hi() {
            return shift_hi_mutation(&rhs, rhs_range.hi() - lhs_range.hi())
                .map(|m| NarrowResult::Narrow(self.rhs, m))
                .unwrap_or(NarrowResult::Violation);
        }
        if rhs_range.lo() < lhs_range.lo() {
            return shift_lo_mutation(&rhs, lhs_range.lo() - rhs_range.lo())
                .map(|m| NarrowResult::Narrow(self.rhs, m))
                .unwrap_or(NarrowResult::Violation);
        }

        // values not on the right hand side
        let rhs_only = {
            let mut rhs_only = rhs.clone();
            rhs_only.set_subtract_mut(&lhs);
            rhs_only
        };
        // dbg!(&rhs_only);

        // TODO: we need to make sure all elements in rhs_only are divisible by lhs_coefficient
        //       i.e. it's actually possible to have lhs * lhs_coefficient = each value
        if rhs_only.is_empty() {
            NarrowResult::Satisfied
        } else {
            NarrowResult::Narrow(self.rhs, Mutation::Exclude { values: rhs_only })
        }
    }
}

impl Constraint for MultiplyConstEqConstraint {
    fn propogate(&self, ctx: &mut PropogationEnvironment, event: Option<&Event>) -> NarrowResult {
        match event {
            Some(event) if event.variable == self.rhs => {
                let lhs_result = self.narrow_lhs(ctx);
                if matches!(
                    lhs_result,
                    NarrowResult::Violation | NarrowResult::Satisfied
                ) {
                    self.narrow_rhs(ctx)
                } else {
                    lhs_result
                }
            }
            Some(_) | None => {
                let rhs_result = self.narrow_rhs(ctx);
                if matches!(
                    rhs_result,
                    NarrowResult::Violation | NarrowResult::Satisfied
                ) {
                    self.narrow_lhs(ctx)
                } else {
                    rhs_result
                }
            }
        }
    }
}
