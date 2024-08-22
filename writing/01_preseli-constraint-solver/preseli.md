# Integer Constraints.

In this last week or so, I wrote something which I think you'd call a "Constraint Solver".
It's called *Preseli*, a fairly low-in-the-stack component of the language I'm working on for school, Howlite.

Let's talk about placing constraints on integer variables.

## The Problem

Say, `0 ≤ x ≤ 20`, `17 ≤ y ≤ 30`, and `3 ≤ z ≤ 5`.
Futhermore, we'd like `x < y < z * 5`.

We can break this problem down a bit:

1. `z = 3`, `4`, or `5`. so `z * 5 = 15`, `20`, or `25`.
2. So, `y ≤ z * 5` implies `y < 15`.
3. But wait! `17 ≤ y`. So, we can instead, let's say `17 ≤ y < 20`.
4. Now `17 ≤ y < 20 ≤ z * 5`, means `20 / 5 ≤ z`. So, `4 ≤ z ≤ 5`.
5. Finally, let's look at x: `0 ≤ x ≤ y`, since `y ≤ 17`, `x` must be `0 ≤ x < 17`.

Finally, the answer: `0 ≤ x ≤ 16`, `17 ≤ y ≤ 20`, `4 ≤ z ≤ 5`.

### Road not Taken

Or, that's one answer. of course, `x = 0`, `y = 21`, `z = 5` would also work.
Why this answer? Well, working through the problem, writing this post, It's the one I happened find.

Given this problem, Preseli minimizes `y`, so `y = 17`. Which isn't ideal, if we want to maximize the domain of each variable.

### Choosing Solutions

This is a search problem. A binary search is a much smaller, but conceptually similar algorithm. We have a domain, and we recursively narrow that domain until a solution is found, or we've eliminated the entire domain.

Now, in our problem we have several variables; all of which may have any set of integer values. There are many integers, even if constrained by our computer's memory. And there are many more possible sets of integers.