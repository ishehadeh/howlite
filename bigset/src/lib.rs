pub mod bitfield;
pub mod ops;
pub mod range;
pub mod step_range;
pub mod stripeset;

use std::{
    cell::RefCell,
    collections::{BTreeMap, VecDeque},
    fmt::{write, Debug},
    num::NonZeroUsize,
    ops::Bound,
    sync::Arc,
};

use num_prime::{
    self,
    buffer::{NaiveBuffer, PrimeBufferExt},
    detail::{PrimalityBase, PrimalityRefBase},
    Primality,
};

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub enum StateRef {
    Accept,
    Go(NonZeroUsize),
}

#[derive(Debug, Clone)]
pub struct FactorState<I: num_integer::Integer> {
    transitions: Vec<(I, StateRef)>,
}

#[derive(Copy, Clone, Ord, Debug, PartialEq, PartialOrd, Eq)]
struct State(usize);

impl State {
    pub fn is_accept(&self) -> bool {
        self.0 == usize::MAX
    }

    pub const fn accept() -> Self {
        Self(usize::MAX)
    }
}
#[derive(Clone)]
pub struct FactorSet<
    I: num_integer::Integer + PrimalityBase + Debug,
    PrimeBufT: for<'a> num_prime::PrimeBuffer<'a> = num_prime::buffer::NaiveBuffer,
> where
    for<'r> &'r I: PrimalityRefBase<I>,
{
    primes: Arc<RefCell<PrimeBufT>>,
    states: BTreeMap<(State, I, usize), State>,
    initial_state: State,
    last_inserted_state: State,
    counter: usize,
}

impl<
        I: num_integer::Integer + PrimalityBase + Debug,
        PrimeBufT: for<'a> num_prime::PrimeBuffer<'a>,
    > std::fmt::Debug for FactorSet<I, PrimeBufT>
where
    for<'r> &'r I: PrimalityRefBase<I>,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "FactorSet {{")?;
        if f.alternate() {
            writeln!(f)?
        }

        for ((from, prime, c), to) in &self.states {
            if f.alternate() {
                writeln!(f, "    ({from:?}, {prime:?}, {c:?}) -> {to:?};")?
            } else {
                write!(f, " ({from:?}, {prime:?}, {c:?}) -> {to:?};")?
            }
        }

        write!(f, "}}")
    }
}
impl<
        I: num_integer::Integer + PrimalityBase + Debug,
        PrimeBufT: for<'a> num_prime::PrimeBuffer<'a>,
    > FactorSet<I, PrimeBufT>
where
    for<'r> &'r I: PrimalityRefBase<I>,
{
    pub fn new(primes: Arc<RefCell<PrimeBufT>>) -> FactorSet<I, PrimeBufT> {
        FactorSet {
            states: Default::default(),
            primes,
            initial_state: State(0),
            last_inserted_state: State(0),
            counter: 0,
        }
    }

    #[inline(always)]
    fn iter_state(&self, state: State) -> impl Iterator<Item = (&(State, I, usize), &State)> {
        self.states.range((
            Bound::Included((state, I::zero(), 0)),
            Bound::Included((State(state.0 + 1), I::zero(), 0)),
        ))
    }

    fn add_state(&mut self, parent: State, prime: I, accept: bool) -> State {
        let next = if accept {
            State::accept()
        } else {
            State(self.last_inserted_state.0 + 1)
        };
        if next != State::accept() {
            self.last_inserted_state = next;
        }
        self.states.insert((parent, prime, self.counter), next);
        self.counter += 1;
        next
    }

    fn add_transition(&mut self, from: State, prime: I, to: State) -> State {
        self.states.insert((from, prime, self.counter), to);
        self.counter += 1;
        to
    }

    fn init_new_root(&mut self) -> State {
        let root = State(self.last_inserted_state.0 + 1);
        self.last_inserted_state = root;
        self.initial_state = root;
        root
    }

    fn gen_state(&mut self) -> State {
        let root = State(self.last_inserted_state.0 + 1);
        self.last_inserted_state = root;
        self.initial_state = root;
        root
    }

    pub fn include(&mut self, n: I) {
        let (rem, final_existing_state) = self.follow(n);

        if !rem.is_one() {
            let primes = self.primes.borrow_mut().factorize(rem).into_iter();
            let nprimes = primes.len();

            let mut current_state = final_existing_state;
            for (i, (prime, exp)) in primes.enumerate() {
                for j in 0..exp {
                    current_state = self.add_state(
                        current_state,
                        prime.clone(),
                        i == nprimes - 1 && j == exp - 1,
                    );
                }
            }
        }
    }

    fn rec_sort_state(&mut self, cond: &(State, I, usize), nth: usize) -> bool {
        let (local_cond, to) = match self.iter_state(*self.states.get(cond).unwrap()).nth(nth) {
            Some((a, b)) => (a.clone(), *b),
            None => return false,
        };

        if to != State::accept() {
            self.rec_sort_state(&local_cond, 0);
        }

        if local_cond.1 < cond.1 {
            let local_to = self.states.remove(&local_cond).unwrap();
            let parent_to = self.states.remove(cond).unwrap();
            self.states
                .insert((local_cond.0, cond.1.clone(), cond.2), local_to);
            self.states
                .insert((cond.0, local_cond.1, local_cond.2), parent_to);
            return local_to == State::accept();
        }
        self.rec_sort_state(cond, nth + 1)
    }

    pub fn multiply(&mut self, n: I) {
        let primes = self.primes.borrow_mut().factorize(n).into_iter();
        let nprimes = primes.len();

        let old_root = self.initial_state;
        let mut current_state = self.init_new_root();
        for (i, (prime, exp)) in primes.enumerate() {
            for j in 0..exp {
                let to = if i == nprimes - 1 && j == exp - 1 {
                    old_root
                } else {
                    self.gen_state()
                };
                // println!("({:?}, {:?}) -> {:?}", current_state, &prime, to);
                current_state = self.add_transition(current_state, prime.clone(), to);
            }
        }
    }

    pub fn includes(&self, n: I) -> bool {
        // FIXME: this doesn't work, we need to backtrack, or know prime factors up front
        self.follow(n).0.is_one()
    }

    fn follow(&self, n: I) -> (I, State) {
        let mut x = n;
        let mut found_transition = true;
        let mut trace = VecDeque::new();
        trace.push_back((self.initial_state, I::zero(), 0));
        println!("follow: {:?}", x);
        while let Some(current_state) = trace.back().cloned() {
            if current_state.0 == State::accept() {
                break;
            }

            found_transition = false;
            for ((state, prime, c), next) in self.states.range((
                Bound::Included(current_state.clone()),
                Bound::Excluded((State(current_state.0 .0 + 1), I::zero(), 0)),
            )) {
                let (div, rem) = x.div_mod_floor(prime);
                if rem.is_zero() {
                    x = div;
                    println!(
                        "  ({:?}, {:?}) -> {:?} [x={:?}]",
                        current_state, &prime, next, &x
                    );
                    if !x.is_one() && next == &State::accept() {
                        // backtrack
                        trace.pop_back();
                    } else {
                        trace.push_back((*state, prime.clone(), *c + 1));
                        trace.push_back((*next, I::one(), 0));
                        found_transition = true;
                    }
                    break;
                }
            }

            if !found_transition {
                println!("   no transitions, options: [{current_state:?}]");
                for ((from, prime, c), to) in self.states.range((
                    Bound::Included(current_state.clone()),
                    Bound::Excluded((State(current_state.0 .0 + 1), I::zero(), 0)),
                )) {
                    println!(
                        "    ({:?}, {:?}, {:?}) -> {:?} [x={:?}]",
                        from, &prime, c, to, &x
                    );
                }
                trace.pop_back();
            }
        }

        (x, trace.back().map(|x| x.0).unwrap_or(self.initial_state))
    }
}

#[test]
fn factor_set() {
    let prime_buf = Arc::new(RefCell::new(NaiveBuffer::new()));
    let mut set = FactorSet::new(prime_buf);
    assert!(!set.includes(10000_usize));
    set.include(52_usize);
    set.include(9_usize);
    dbg!(&set);
    assert!(set.includes(52_usize));
    assert!(set.includes(9_usize));
    assert!(!set.includes(207_usize));
    set.include(10_usize);
    dbg!(&set);
    set.multiply(12_usize);
    dbg!(&set);
    let first = set
        .iter_state(set.initial_state)
        .next()
        .map(|a| *a.0)
        .unwrap();
    set.rec_sort_state(&first, 0);
    dbg!(&set);
    assert!(set.includes(9_usize * 12));
    assert!(set.includes(1_usize));

    // (p1 p2 p3) + (p1 p4 p7) = p1 ( (p2 p3) + (p4 p7) )
}
