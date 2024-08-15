use crate::{integer::IntegerRange, Constraint, ConstraintContext, Event, Mutation, Variable};

#[derive(Debug, Clone)]
pub struct BinaryAddConstraint {
    pub x: Variable,
    pub y: Variable,
    pub sum: Variable,
}

impl BinaryAddConstraint {
    pub fn new(x: Variable, y: Variable, sum: Variable) -> BinaryAddConstraint {
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
            if x_hi_diff + y_hi_diff != -&hi_diff {
                todo!("constraint failed");
            }

            x_range.hi = new_x_hi;
            y_range.hi = new_y_hi;
        }

        (x_range, y_range)
    }

    fn adjust_sum_bound(&self, mut ctx: ConstraintContext, new_bound: IntegerRange) {
        let x_domain = ctx.variables.get(self.x);
        let y_domain = ctx.variables.get(self.y);
        let x_range = x_domain.range().unwrap();
        let y_range = y_domain.range().unwrap();

        let (new_x_range, new_y_range) = self.constrain_sum(&x_range, &y_range, &new_bound);
        if x_range != new_x_range {
            ctx.submit(self.x, Mutation::Bound { range: new_x_range });
        }

        if y_range != new_y_range {
            ctx.submit(self.y, Mutation::Bound { range: new_y_range });
        }
    }

    fn adjust_xy_bound(&self, mut ctx: ConstraintContext, variable: Variable) {
        let x_domain = ctx.variables.get(self.x);
        let y_domain = ctx.variables.get(self.y);
        let sum_domain = ctx.variables.get(self.sum);
        let x_range = x_domain.range().unwrap();
        let y_range = y_domain.range().unwrap();
        let sum_range = sum_domain.range().unwrap();

        let (mutated_range, other_range, other_var) = if variable == self.x {
            (x_range, y_range, self.y)
        } else {
            (y_range, x_range, self.x)
        };
        let (new_sum_range, new_other_range) =
            self.constrain_difference(&sum_range, &other_range, &mutated_range);
        if new_other_range != other_range {
            ctx.submit(
                other_var,
                Mutation::Bound {
                    range: new_other_range,
                },
            );
        }

        if sum_range != new_sum_range {
            ctx.submit(self.sum, Mutation::Bound { range: sum_range });
        }
    }
}

impl Constraint for BinaryAddConstraint {
    fn propogate(&mut self, ctx: ConstraintContext, event: Event) {
        match event.mutation {
            Mutation::Instantiate { binding } => todo!(),
            Mutation::Bound { range } if event.subject == self.sum => {
                self.adjust_sum_bound(ctx, range);
            }
            Mutation::Bound { range: _ } if event.subject == self.x || event.subject == self.y => {
                self.adjust_xy_bound(ctx, event.subject);
            }
            Mutation::Bound { range } => todo!(),
            Mutation::Exclude { excluded } => todo!(),
        }
    }

    fn initialize(&mut self, ctx: crate::ConstraintContext) {
        // TODO: better initial constraint setup
        self.adjust_xy_bound(ctx, self.x);
    }
}
