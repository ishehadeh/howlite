use crate::{
    environment::{Constraint, PropogationEnvironment},
    integer::IntegerRange,
    variables::{Mutation, VariableId},
};

#[derive(Debug, Clone)]
pub struct BinaryAddConstraint {
    pub x: VariableId,
    pub y: VariableId,
    pub sum: VariableId,
}

impl BinaryAddConstraint {
    pub fn new(x: VariableId, y: VariableId, sum: VariableId) -> BinaryAddConstraint {
        BinaryAddConstraint { x, y, sum }
    }
}

impl BinaryAddConstraint {
    fn constrain_difference(
        &self,
        a: &IntegerRange,
        b: &IntegerRange,
        within: &IntegerRange,
    ) -> (IntegerRange, IntegerRange) {
        let mut x_range = a.clone();
        let mut y_range = b.clone();

        let lo_diff = (x_range.lo.clone() - y_range.lo.clone()) - &within.lo;
        if lo_diff > 0.into() {
            // x or y need to have a higher minimum.
            // we'll exclude x as much as possible, then y.

            let new_x_lo = (&x_range.lo + (-&lo_diff)).min(x_range.hi.clone());
            let x_lo_diff = &new_x_lo - &x_range.lo;
            let new_y_lo = (&y_range.lo + (-&lo_diff) - &x_lo_diff).min(y_range.hi.clone());
            let y_lo_diff = &new_y_lo - &y_range.lo;
            if x_lo_diff + y_lo_diff != -lo_diff {
                todo!("constraint failed");
            }

            x_range.lo = new_x_lo;
            y_range.lo = new_y_lo;
        }

        let hi_diff = (&x_range.hi - &y_range.hi) - &within.hi;
        if hi_diff < 0.into() {
            // x or y need to have a lower maximum.
            // we'll exclude x as much as possible, then y.

            let new_x_hi = (&x_range.hi + (-&hi_diff)).max(x_range.lo.clone());
            let x_hi_diff = &new_x_hi - &x_range.hi;
            let new_y_hi = (&y_range.hi + (-&hi_diff) - &x_hi_diff).max(y_range.lo.clone());
            let y_hi_diff = &new_y_hi - &y_range.hi;
            dbg!(&hi_diff);
            dbg!(&new_x_hi);
            dbg!(&new_y_hi);
            if x_hi_diff + y_hi_diff != -&hi_diff {
                todo!("constraint failed");
            }

            x_range.hi = new_x_hi;
            y_range.hi = new_y_hi;
        }

        (x_range, y_range)
    }

    fn constrain_sum(
        &self,
        a: &IntegerRange,
        b: &IntegerRange,
        within: &IntegerRange,
    ) -> (IntegerRange, IntegerRange) {
        let mut x_range = a.clone();
        let mut y_range = b.clone();

        let lo_diff = (x_range.lo.clone() + y_range.lo.clone()) - &within.lo;
        if lo_diff < 0.into() {
            // x or y need to have a higher minimum.
            // we'll exclude x as much as possible, then y.

            let new_x_lo = (&x_range.lo + (-&lo_diff)).min(x_range.hi.clone());
            let x_lo_diff = &new_x_lo - &x_range.lo;
            let new_y_lo = (&y_range.lo + (-&lo_diff) - &x_lo_diff).min(y_range.hi.clone());
            let y_lo_diff = &new_y_lo - &y_range.lo;
            if x_lo_diff + y_lo_diff != -lo_diff {
                todo!("constraint failed");
            }

            x_range.lo = new_x_lo;
            y_range.lo = new_y_lo;
        }

        let hi_diff = (&x_range.hi + &y_range.hi) - &within.hi;
        if hi_diff > 0.into() {
            // x or y need to have a lower maximum.
            // we'll exclude x as much as possible, then y.

            let new_x_hi = (&x_range.hi + (-&hi_diff)).max(x_range.lo.clone());
            let x_hi_diff = &new_x_hi - &x_range.hi;
            let new_y_hi = (&y_range.hi + (-&hi_diff) - &x_hi_diff).max(y_range.lo.clone());
            let y_hi_diff = &new_y_hi - &y_range.hi;
            if &x_hi_diff + &y_hi_diff != -&hi_diff {
                dbg!(&x_hi_diff);
                dbg!(&y_hi_diff);
                dbg!(&hi_diff);
                todo!("constraint failed");
            }

            x_range.hi = new_x_hi;
            y_range.hi = new_y_hi;
        }

        (x_range, y_range)
    }

    fn adjust_sum_bound(&self, mut ctx: &mut PropogationEnvironment, new_bound: IntegerRange) {
        let x_range = ctx.variable_range(self.x).unwrap();
        let y_range = ctx.variable_range(self.y).unwrap();

        let (new_x_range, new_y_range) = self.constrain_sum(&x_range, &y_range, &new_bound);
        if x_range != new_x_range {
            ctx.mutate(self.x, Mutation::BoundLo { lo: new_x_range.lo });
            ctx.mutate(self.x, Mutation::BoundHi { hi: new_x_range.hi });
        }

        if y_range != new_y_range {
            ctx.mutate(self.y, Mutation::BoundLo { lo: new_y_range.lo });
            ctx.mutate(self.y, Mutation::BoundHi { hi: new_y_range.hi });
        }
    }

    fn adjust_xy_bound(&self, ctx: &mut PropogationEnvironment, variable: VariableId) {
        let x_range = ctx.variable_range(self.x).unwrap();
        let y_range = ctx.variable_range(self.y).unwrap();
        let sum_range = ctx.variable_range(self.sum).unwrap();

        let (mutated_range, other_range, other_var) = if variable == self.x {
            (x_range, y_range, self.y)
        } else {
            (y_range, x_range, self.x)
        };
        let (new_sum_range, new_other_range) =
            self.constrain_difference(&sum_range, &other_range, &mutated_range);
        if new_other_range != other_range {
            ctx.mutate(
                other_var,
                Mutation::BoundLo {
                    lo: new_other_range.lo,
                },
            );
            ctx.mutate(
                other_var,
                Mutation::BoundHi {
                    hi: new_other_range.hi,
                },
            );
        }

        if sum_range != new_sum_range {
            ctx.mutate(self.sum, Mutation::BoundLo { lo: sum_range.lo });
            ctx.mutate(self.sum, Mutation::BoundHi { hi: sum_range.hi });
        }
    }
}

impl Constraint for BinaryAddConstraint {
    fn propogate(&mut self, ctx: &mut PropogationEnvironment) {
        match ctx.last_mutation() {
            None => self.adjust_sum_bound(ctx, ctx.variable_range(self.sum).unwrap()),
            Some((var, Mutation::BoundLo { .. } | Mutation::BoundHi { .. })) => {
                if var == self.sum {
                    self.adjust_sum_bound(ctx, ctx.variable_range(self.sum).unwrap())
                } else {
                    self.adjust_xy_bound(ctx, var)
                }
            }
            Some((_, _)) => (),
        }
    }
}
