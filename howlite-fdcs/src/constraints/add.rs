use std::cmp::Ordering;

use num_bigint::BigInt;

use crate::{
    environment::{Constraint, PropogationEnvironment},
    integer::{IntegerRange, RangeSide},
    variables::{Mutation, VariableId},
};

#[derive(Debug, Clone)]
pub struct BinaryAddConstraint {
    pub x: VariableId,
    pub x_coefficient: BigInt,
    pub y: VariableId,
    pub sum: VariableId,
}

impl BinaryAddConstraint {
    pub fn new(x: VariableId, y: VariableId, sum: VariableId) -> BinaryAddConstraint {
        BinaryAddConstraint {
            x,
            x_coefficient: 1.into(),
            y,
            sum,
        }
    }

    pub fn new_with_coefficient(
        x: VariableId,
        x_coefficient: BigInt,
        y: VariableId,
        sum: VariableId,
    ) -> BinaryAddConstraint {
        BinaryAddConstraint {
            x,
            x_coefficient,
            y,
            sum,
        }
    }
}

impl BinaryAddConstraint {
    fn x_range_with_coefficient(&self, ctx: &PropogationEnvironment) -> IntegerRange {
        let mut x_range = ctx.variable_range(self.x).unwrap();
        x_range.lo *= &self.x_coefficient;
        x_range.hi *= &self.x_coefficient;
        x_range
    }

    fn div_coefficient_with_remainder(&self, x: &BigInt, round_up: bool) -> (BigInt, BigInt) {
        let div = x / &self.x_coefficient;
        let rounded = &div * &self.x_coefficient;
        if &div == &rounded {
            (div, BigInt::ZERO)
        } else if round_up && &rounded < x {
            (div + 1, rounded + &self.x_coefficient - x)
        } else if !round_up && &rounded > x {
            (div - 1, rounded - &self.x_coefficient - x)
        } else {
            (div, rounded - &self.x_coefficient)
        }
    }

    fn constrain_lhs_range(
        &self,
        ctx: &mut PropogationEnvironment,
        side: RangeSide,
        vars: &[VariableId],
    ) -> bool {
        println!("constrain_lhs_range(ctx, side: {side:?}, vars: {vars:?})");
        let sum_range = ctx.variable_range(self.sum).unwrap();
        let x_range_coeff = self.x_range_with_coefficient(ctx);
        let y_range = ctx.variable_range(self.y).unwrap();
        // how much we need to shift the upper bound down, or the lower bound up of cX + Y to fit withing the bounds of Sum.
        let mut adjustment_needed = if side == RangeSide::Hi {
            (&x_range_coeff.hi + &y_range.hi - &sum_range.hi)
        } else {
            (&sum_range.lo - (&x_range_coeff.lo + &y_range.lo))
        };
        for &variable in vars {
            match adjustment_needed.cmp(&BigInt::ZERO) {
                // can't make x/y bigger
                Ordering::Less if variable != self.sum => continue,
                Ordering::Equal => break,
                // can't make sum larger
                Ordering::Greater if variable == self.sum => continue,
                _ => (),
            };

            dbg!(variable, &adjustment_needed);
            let var_range = ctx.variable_range(variable).unwrap();
            let adjustment = if variable == self.x {
                let (adjustment_div_coeff, _) =
                    self.div_coefficient_with_remainder(&adjustment_needed, side != RangeSide::Lo);
                let adjustment = var_range.size().min(adjustment_div_coeff);
                adjustment_needed -= &adjustment * &self.x_coefficient;
                adjustment
            } else if variable == self.y {
                let adjustment = var_range.size().min(adjustment_needed.clone());
                adjustment_needed -= &adjustment;
                adjustment
            } else if variable == self.sum {
                let adjustment = var_range.size().min(-adjustment_needed.clone());
                adjustment_needed += &adjustment;
                adjustment
            } else {
                // not one of our variables
                continue;
            };
            dbg!(&adjustment, &adjustment_needed);

            if side == RangeSide::Hi {
                ctx.mutate(
                    variable,
                    Mutation::BoundHi {
                        hi: var_range.hi - adjustment,
                    },
                )
                .unwrap();
            } else {
                ctx.mutate(
                    variable,
                    Mutation::BoundLo {
                        lo: var_range.lo + adjustment,
                    },
                )
                .unwrap();
            }
        }

        dbg!(&adjustment_needed);

        // adjustments < 0 indicate that the sum range is larger than needed.
        // this is ok, the constraint is still valid.
        adjustment_needed <= BigInt::ZERO
    }
}

impl Constraint for BinaryAddConstraint {
    fn propogate(&mut self, ctx: &mut PropogationEnvironment) -> bool {
        match ctx.last_mutation() {
            None => {
                self.constrain_lhs_range(ctx, RangeSide::Hi, &[self.x, self.y, self.sum])
                    && self.constrain_lhs_range(ctx, RangeSide::Lo, &[self.x, self.y, self.sum])
            }
            Some((var, Mutation::BoundLo { .. } | Mutation::BoundHi { .. })) => {
                let vars = if var == self.sum {
                    &[self.x, self.y]
                } else if var == self.x {
                    &[self.y, self.sum]
                } else if var == self.y {
                    &[self.x, self.sum]
                } else {
                    &[self.x, self.y]
                };

                self.constrain_lhs_range(ctx, RangeSide::Hi, vars)
                    && self.constrain_lhs_range(ctx, RangeSide::Lo, vars)
            }
            Some((_, _)) => true,
        }
    }
}
