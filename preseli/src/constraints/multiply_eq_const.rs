use crate::{
    environment::{Constraint, Event, NarrowResult, PropogationEnvironment},
    integer::{num_bigint::BigInt, HI, LO},
    variables::{Mutation, Variable, VariableId},
};
use num_integer::Integer;

#[derive(Clone, Debug)]
pub struct MultiplyConstEqConstraint {
    pub lhs: VariableId,
    pub lhs_coefficient: BigInt,
    pub rhs: VariableId,
}

impl MultiplyConstEqConstraint {
    pub fn new(
        lhs: VariableId,
        lhs_coefficient: BigInt,
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

        let lhs = lhs_var.clone() * &self.lhs_coefficient;
        let rhs = match ctx.variable(self.rhs) {
            Variable::Instantiated(x) => x,
            Variable::Domain(d) => d,
        };

        let lhs_range = lhs.range().unwrap();
        let lhs_var_range = lhs_var.range().unwrap();
        let rhs_range = rhs.range().unwrap();
        if lhs_range != rhs_range && !is_mutable {
            return NarrowResult::Violation;
        }
        dbg!(&lhs_range);

        if lhs_range.hi > rhs_range.hi {
            return lhs_var_range
                .outward_shift_mutation(
                    HI,
                    (&rhs_range.hi - &lhs_range.hi)
                        .max(-lhs_range.size())
                        .div_floor(&self.lhs_coefficient),
                )
                .map(|m| NarrowResult::Narrow(self.lhs, m))
                .unwrap_or(NarrowResult::Violation);
        }
        if lhs_range.lo < rhs_range.lo {
            dbg!((&lhs_range.lo - &rhs_range.lo).max(-lhs_range.size()));
            return lhs_var_range
                .outward_shift_mutation(
                    LO,
                    (&lhs_range.lo - &rhs_range.lo)
                        .max(-lhs_range.size())
                        .div_floor(&self.lhs_coefficient),
                )
                .map(|m| NarrowResult::Narrow(self.lhs, m))
                .unwrap_or(NarrowResult::Violation);
        }

        let lhs_only = {
            let mut lhs_only = lhs.clone();
            lhs_only.subtract(rhs);
            lhs_only
        };

        // TODO: performance
        if let Some(diff) = lhs_only.spans().next() {
            return NarrowResult::Narrow(
                self.lhs,
                Mutation::Exclude {
                    value: diff.lo.clone() / &self.lhs_coefficient,
                },
            );
        }

        NarrowResult::Satisfied
    }

    fn narrow_rhs(&self, ctx: &mut PropogationEnvironment) -> NarrowResult {
        let (is_mutable, rhs) = match ctx.variable(self.rhs) {
            Variable::Instantiated(d) => (false, d.clone()),
            Variable::Domain(d) => (true, d.clone()),
        };

        let lhs = match ctx.variable(self.lhs) {
            Variable::Instantiated(x) => x.clone(),
            Variable::Domain(d) => d.clone() * &self.lhs_coefficient,
        };

        let lhs_range = lhs.range().unwrap();
        let rhs_range = rhs.range().unwrap();
        if lhs_range != rhs_range && !is_mutable {
            return NarrowResult::Violation;
        }

        if rhs_range.hi > lhs_range.hi {
            return rhs_range
                .outward_shift_mutation(HI, (&lhs_range.hi - &rhs_range.hi).max(-rhs_range.size()))
                .map(|m| NarrowResult::Narrow(self.rhs, m))
                .unwrap_or(NarrowResult::Violation);
        }
        if rhs_range.lo < lhs_range.lo {
            return rhs_range
                .outward_shift_mutation(LO, (&rhs_range.lo - &lhs_range.lo).max(-rhs_range.size()))
                .map(|m| NarrowResult::Narrow(self.rhs, m))
                .unwrap_or(NarrowResult::Violation);
        }

        let rhs_only = {
            let mut rhs_only = rhs.clone();
            rhs_only.subtract(&lhs);
            rhs_only
        };
        dbg!(&rhs_only);

        // TODO: performance
        if let Some(diff) = rhs_only.spans().next() {
            if &diff.lo % &self.lhs_coefficient == BigInt::ZERO {
                return NarrowResult::Narrow(
                    self.rhs,
                    Mutation::Exclude {
                        value: diff.lo.clone(),
                    },
                );
            } else {
                return NarrowResult::Violation;
            }
        }

        NarrowResult::Satisfied
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
