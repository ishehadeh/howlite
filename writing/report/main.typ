#import "@preview/classic-jmlr:0.4.0": jmlr
#import "@preview/wrap-it:0.1.0": wrap-content
#import "@preview/fletcher:0.5.1" as fletcher: diagram, node, edge
#import "./util.typ": *


#let affls = (
  smcm: (
    department: "Department of Math and Computer Science",
    institution: "St. Mary's College of Maryland",
    location: "St. Mary's City, Maryland",
    country: "USA"),
)

#let authors = (
  (name: "Ian Shehadeh",
   affl: "smcm",
   email: "irhshehadeh@smcm.edu"),
)

#show: jmlr.with(
  title: [A Low Level Language with Precise Integer Types],
  authors: (authors, affls),
  abstract: include "./snippets/abstract.typ",
  keywords: ("programming language",),
  bibliography: par(first-line-indent: 0em, bibliography("main.bib")),
  appendix: include "snippets/appendix.typ",
)


// Audience expectations: some familiarity with programming languages, possibly some familiarity with set theory


= Programming Language Concepts and Motivation

_Programming Language_ is a broad term, generally a programming language is a text-based format for expressing computation.
Although most programs are written by software engineers, and most programming languages are designed with software engineers in mind, this by no means makes them a homogenous group.
Once a program is written it is read by both a machine (the compiler or interpreter) and humans.
To satisfy both audiences, it must be clear in two ways: first, it needs to be unambiguous so the machine can produce consistent and accurate results; second, it must be expressive, meaning the author's intent should be apparent. To give context about what choices language designers might make, depending on their audience, we introduce _Hedy_, and _Go_.

== Hedy

#wrapped-figure(
    left: include "examples/hedy.typ",
    right: [
      Hedy [@hedy] is a programming language for teaching programming.
      The language avoids symbols, instead using keywords, which are generally easier for students to remember. With so much of the language being textual, Hedy is fully translated to a large set of languages, 47 at the time of writing. The programs in both @ex-hedy-english and @ex-hedy-arabic print "Hello!" and ask the user their name, but their keywords are in different languages. Hedy also allows programmers to see and hear the results of their work: it has easily accessible functionality for playing music and drawing graphics. Those features are typically implemented as libraries for most programming languages since they have a relatively narrow application.
    ],
    under: [
      In most programming languages, getting rich feedback from a program requires using complex graphics and audio libraries. To help new programmers see results quickly Hedy bundles graphics and audio output into the core language.
    ])

== Go

#wrapped-figure(
  right: include "examples/go.typ",
  left: [
    Go was an answer to problems with the software infrastructure at Google [@pike_GoAtGoogle].
    It's designed to be used in large, long-lived software projects. There's a focus on clear syntax and semantics: no matter who wrote the code, the problem this program solves and the algorithms used to solve it should be apparent. Go also comes bundled with tools to keep programs up to date and consistent.
    For example, unused variables and imports are disallowed. Although it supports first class functions, it's largely imperative.
  ],
  under: [
    Programmers are forced to deal with the inherent complexity of things like string encoding up front, as seen in @ex-go.
  ]
)


== Howlite's Purpose

#wrapped-figure(
  left: include "examples/howlite-general.typ",
  right: [

Howlite is an expiremental language for writing programs that necessitate little abstraction over they machines they control. The project's goal is to answer the question, _How can we create an expressive type system without limiting a programmer's control of the hardware?_

])

= The Programming Language

Memory safety in systems programming languages has garnered a lot of attention in the last several years. 
A compiler that enforces strict rules on an object's lifetime and mutability is helpful in large projects, especially when security is a top concern. Checking these properties at compile time allows the compiler to omit parts of its runtime, like a garbage collector, while providing similar guarantees.

These innovations in language design fail to directly address a class of problems where direct memory manipulation is essential. These problems force the programmer to fully disable the compiler's checks, or encourage awkward solutions that trade clarity for small guarantees. 

Howlite aims to address these problems. Howlite is not a language to write a web server, it is not for writing applications, it isn't even a language for writing programming languages. It is a language for writing a single module for a specific data structure, wrapped in a Python library. It is a language for writing a bootloader or the entry point to a kernel. The compiler does not impose strict requirements on how the programmer manages memory or accesses data. Instead, the type systems provide a rich set of tools, that enable programmers to precisely describe how data is transformed.


#wrapped-figure(
  right: include "examples/boolean.typ",
  left: [
== Overview

The most notable feature of Howlite is the type system. The type system is structural and closely tracks the value of integers. For example, you can declare types that only allow the values `1`, `2`, or `5`. Types are compared based on their compatibility, not by name. For example, the data structure `{ x: int, y: int, z: int }` is compatible with
],
  under: [
    the type `{ x: int, y: int }`. To better understand the language, this section will walk through the process of defining a function to get the index of a character from an ASCII string.
  ]
)


// TODO: the transition here is a bit sudden, do we need a sentance prior to type defs?
#v(1em)
#wrapped-figure(
  left: include "examples/index_of/typedefs.typ",
  right: [
    First, we define a character as any number between `0` and `127` (i.e. 7-bit ASCII characters).
    Next is the definition of a standard 32-bit integer, which is used to index the array.
    Finally, we define a variant of `i32` that is only positive.
    We'll use this type to represent the index, which can't be negative.
  ],
)


Now, we move on to the function signature. The syntax will be familiar to Go programmers, with a few small changes.
#include "examples/index_of/signature.typ"


This function is generic, the `[LenT: NatI32]` section indicates that for any subset of the positive, signed, 32-bit integers, there is an instance of `index_of`.
Whatever that type is, it is referred to as `LenT` within the context of this function.

Moving on to the parameter list, notice the type of `str` is `&[char; LenT]`.
This `&[...]` is a special type called a _slice_ (also known as a fat pointer).
A slice is simply a pointer and length pair; practically it functions like an array.
Slice types are common, they're primitives in Rust, Go, and Zig.
Although it's not a primitive type, the C++ STL's `std::span` is a similar data structure.
What sets our slice type apart is that the type of the length can be set.
For example, say we take a slice of some ASCII string, from index 3 to 10, the result would have the type `&[char; 7]`.

By using a generic parameter, `LenT`, then giving `str` the type `&[char; LenT]`.
We can be certain this function only works on a string of length less than or equal to 0x7fffffff.
Since it's impossible to find a character outside of those bounds, we know the return type can't exceed the maximum value of `LenT`, if no character is found then we return `-1`.


#wrapped-figure(
  right: include "examples/index_of/body.typ",
  left: [
   Finally, the body of this function likely looks familiar to C programmers, with some minor syntactic changes. Variables are declared with `let`, `mut` indicates that we can change the value after initialization. All expressions (including `if` statements and blocks) have values. The value of a block is equal to the value of the last line in the block, if it omits a semi-colon (`;`), or `unit` otherwise.

    Some care must be taken to make sure we satisfy the return type.
    How can the compiler be certain `i` is always a subset of `0..Max[LenT]`, since `u32` certainly exceeds `LenT`.
    The answer is that the condition, "`while i < str.len`", narrows `i`'s type from `u32` to `0..LenT-1`.
    This means within the body of that loop, `i` can be used as if it had the type `0..Max[LenT]-1`.
  ],
  under: [
    Arithmetic will modify this type: after running `i = i + 1`, `i`'s narrow type has changed to `1..Max[LenT]`.
    If we changed the code to check some other condition, for example, "`chr < str.len`", this wouldn't compile.
  ]
)


= Syntax Design

#wrapped-figure(
  right: [#include "./examples/syntax-overview.typ"],
  left: [
    Howlite's syntax prioritizes familiarity, ease of parsing, and clarity. The syntax should be immediately familiar to anyone who knows another C-like language. The grammar is context-free, so it can easily be expressed using a parser generator. The language should also clearly reflect exactly what the machine will do when executing the compiled program.
  ])

== Familiarity

Howlite code should be recognizable to C programmers. For this reason, we use curly braces ("{" and "}") to denote blocks of code. We use familiar imperative keywords: "if", "else", and "while", and mathmatical expressions follow typical infix notation. Howlite differs from C in that it requires a sigil character or keyword before beginning a new construct. Types do not lead in variable assignments for functions. Instead we use the "let" or "func" keywords, respectively. These keywords and symbols were decided by picking from popular languages during design. For example, "let", and `:` come from TypeScript, while "func" is a keyword in Go.

== Ease of Parsing

A small, easily parsed grammar is valuable because it makes implementing tooling easier. Anything from simple syntax highlighting in _Emacs_ to an auto-formatter or linter is dramatically easier to implement when parsing the language isn't a significant hurdle.

Howlite's syntax is expressable in an LR grammar. Consequently, the grammar is unambiguous. While writing the grammar, we aimed to reduce look ahead as much as possible. For example, function's type parameters are written `index_of[:u32](...)`, which disambiguates the use of `[...]` from array access.

== Clarity

We use clarity to mean the ease of understanding a program's behavior.
If a program is clear, then the author's original intent should be easily understood by someone familiar with the language. Ultimately, the author of a program is responsible for making their intent clear; the syntax should guide their choices, and give them the tools to express their intent.

We optimize clarity by keeping tokens consistent, for example, colon (`:`) is almost always a way to give _something_ a type, whether that thing is an expression, variable, or a field of a data structure. However, we don't sacrifice familiarity for consistency. Languages like C, C++, Java, Go, and more use curly braces for both structure declarations and statement blocks, so we follow suit.

Being a low-level language, we want to emphasize precisely what the machine is doing.
Howlite programs are written in an imperative style, we expect the programmer to use mutable state, but discourage it when unnecessary by making it opt-in via the `mut` keyword. We also omit short-hand syntax or functions for functional operations, like transforming the content of an array. While these operations are convenient, they can paper over important details like memory allocation. 

For example, flow control constructs, like if statements may have a value. This allows the programmer to clearly show a variable's value is the result of some condition. In order to make tooling easier to write, we prioritize creating an unambiguous grammar, with no constructs that require unbounded look-ahead.g an unambigous grammar, with no constructs that require unbounded look-ahead.

= Type Checking<sc-type-checking>

Howlite implements a simple bi-directional type checker [@dunfieldBidirectionalTyping2020]. Every node in the AST is given a type. An AST node's type is typically derived from it's children's types, through a process called _synthesis_, we call these types _synthesized types_. Many constructs in the language must be ascribed types by the programmer: variables declared with "`let`", function parameters, and return values. Types which are declared explicitly are called _assumed_ types.

#wrapped-figure(
  left: include "examples/let.typ",
  right: [
    Here, `u32` is the assumed type of `x`. Where ever `x` is referenced, we can consider it of type `u32`. The literal `1` has no assumed type. Instead, we synthesize a type for `1` by following a set of rules. For literals, this rule is simple: _for a literal scalar $N$_
  ],
  under: [
    _the synthesized type is $N$_. As expressions grow, synthesizing types becomes more complicated.
  ])

== Type Checking an AST

To better illustrate this process, we'll walk through synthesizing a tree.

#include "examples/average.typ"

The function parameters: `x`, `y`, and `z` are each given the assumed type `0..10`. An assumed type is analogous to the statement "no matter the value of `x`,  we can always assume it is a `0..10`". The function's assumed return type is `0..10`. This allows any caller to treat the expression `average(a, b, c)` as a `0..10`, even if the operations performed by the function are unknown. An assumed type is a promise; it allows references to an entity to _assume_ the type of that entity, without knowing anything else about it.

To illustrate how these assumed types interact with synthesized types, we'll manually type-check the function.

#import "examples/typechecking/colors.typ": noderef-1, noderef-2, noderef-3, noderef-4

#wrap-content(
  pad(right: 8pt, bottom: 8pt, include "./examples/typechecking/full-tree.typ")
)[
 The function body, `(x + y + z) / 3`, has the syntax tree seen in @ast.
The type checker works bottom-up, left to right. So, we begin with the leaves of the tree: #noderef-4(`x`), and #noderef-4(`y`). Identifier AST node's synthesized type is the assumed type of the symbol they include. So #noderef-4(`x`) is synthesized to type `0..10` (the assumed type of `x`), and #noderef-4(`y`) is synthesized to type `0..10` (the assumed type of `y`).

This information is added to the tree, and we reference it to synthesize #noderef-3(`+`). An operator node's synthesized type is constructed by applying the given operation to the synthesized types of each operand. Types may be constructed using arithmetic operations, this process will be defined more formally in @sc-scalars. For now, take for granted that `0..10 + 0..10 : 0..20`.
]

#align(center)[
  #include "examples/typechecking/reduce-1.typ"
]
Now, we move up the tree, to synthesize the right-hand side of #noderef-2(`+`), then finally #noderef-2(`+`) itself.

#align(center)[
  #include "examples/typechecking/reduce-2.typ"
]
In (1) we synthesize the node's type from the assumed type of `z`. In (2) we used this information, and the type of #noderef-3(`+`) to synthesize a type for #noderef-3(`+`).

Finally, we again move up the tree, now to #noderef-1(`/`).

#align(center)[
  #include "examples/typechecking/reduce-3.typ"
]
Due to the function's return value, the assumed type of the body is `0..10`.
A Function body's type is synthesized based on the possible return values.
So, the synthesized type of this function's body is the type of #noderef-1(`/`).

Type checking is the process of comparing assumed and synthesized types.
If a synthesized type is not a subset of the assumed type, then a type error is attached to that node.



= Scalars<sc-scalars>

There is a single scalar type in Howlite, this simplifies the type checking by condensing many cases into a single, generic case. There are no distinct enumerable types, true boolean types, or even a unit type in the language. Instead of distinct types, we have the scalar type "Integer" (floating point numbers are out of scope). 
This collection of types contains any set of integers that can fit within a single general-purpose register on the target architecture.

Going forward, integer types will be expressed using the language's syntax: `1 | 3 | 5` is a type which can be constructed from any of the integers `1`, `3`, or `5`.
The type `1..10` can be constructed from `1`, `10`, or any integer between the two.


== Storage Classes

Scalar types belong to a _storage class_ that identifies how they are encoded in memory.
A storage class defines how many bits the scalar may use, and if one of them is a sign bit.

All integers are assumed to be two's complement.
Consequently all integer overflow and underflow is well defined to wrap.
For example, assuming all numbers in the following expression have a signed, 8-bit storage class, we find `-128 + -128 = 1`.

This mechanism plays well with our concept of scalar types: overflow is allowed, so it doesn't need to be policed if the programmer expects it.
For example, consider a function which averages some set of numbers

#include "examples/average-arbitrary.typ"

It's trivial to overflow cause overflow when adding `acc + nums[i]` (for example, if `nums = [0xffffffff, 0xffffffff]`).
But if the author is concerned more with rapid development, or performance, they may not want to handle this case.

However, if overflow is known to be harmful then it can be explicitly forbidden. For example. Suppose we're reading a 64-bit ELF file, a common executable file format on Unix-like systems, we can read the address and size of a particular section from the $4^"th"$ and $5^"th"$ words of its _Section Heder_ entry (citation needed):

```
let sh_offset: Uint64 = sh_entry[3];
let sh_size: Uint64 = sh_entry[4];
```

We know the file's headers take up at least 184 bytes, and Howlite enables this invariant to be encoded in the type system.

```
let sh_end: 0xB8..Max[Uint64] = sh_offset + sh_size;
```

This fails to compile, since `sh_offset + sh_size` might overflow, and wrap to a number less than both of them.

== Construction of Scalars from Literals

#let cons_c(c, v: none) = {
  let s = "'" + c + "' : 0x" + upper(str((if v == none { c } else {v}).codepoints().at(0).to-unicode(), base: 16))
  raw(s)
}

A scalar is constructed by arithemtic operations, character literals or integer literals.
We will use Howlites own type construction syntax going forward: the given an expression `e` and a type `T`, the expression `e : T` asserts `e` constructs the type `T`.

A literal scalar can be constructed from the a character literal, like `'A'`, `'\n'`, or `'🤯'`. 
The type constructed is a single value, equal to their unicode codepoint. So, #cons_c("A"), #cons_c("\\n", v: "\n"), #cons_c("🤯").

Literals can also be constructed from unsigned integers: `3 : 3`, `5 : 5`, `0b111 : 7`.

== Construction of Scalars from Prefix Operators

The typechecker currently handles the prefix operators `!` (logical not) and `+`, and `-`.
The `+` sign is a no-op, it's included in the language for cases where it might improve clarity.
`-e` constructs the inverse negative of `e`: it's equivalent to the expression `0 - e`.
`!` has three cases: `!a : 0` if the type of `a` does not contain `0`, `!a` is `1` if the type of `a` only contains `0`, and `!a : 0 | 1` otherwise.


#include "examples/scalar-addition.typ"

Given a scalar type $T = {  t_1, t_2, t_3 ... t_n}$, where $forall i : t_i in ZZ$, and a scalar type $U = {  u_1, u_2, u_3 ... u_n}$ where $forall j:u_j in ZZ$. (i.e $T, U$ are subsets of the integers). We can construct the following types:

- $T times U = { t u : forall t in T, forall u in U }$
- $T + U = { t + u : forall t in T, forall u in U }$
- $T - U = { t - u : forall t in T, forall u in U }$
- $T div U = { t div u : forall t in T, forall u in U }$

For example, given $T = { 1, 2, 3}$ and $U = { -5, -7 }$, we'd compute the following:

- $T times U = { 1(-5), 2(-5), 3(-5), 1(-7), 2(-7), 3(-7))} = { -5, -10, -15, -7, -14, -21}$
- $T + U = { 1 + -5, 2 + -5, 3 + -5, 1 + -7 , 2 + -7, 3 + -7) = {-4, -3, -2, -6, -5, -4}$
- $T - U = { 1 - (-5), 2 - (-5), 3 - (-5), 1 - (-7) , 2 - (-7), 3 - (-7)) = {6, 7, 8, 9,10}$
- $T div U = { 1 div (-5), 2 div (-5), 3 div (-5), 1 div (-7) , 2 div (-7), 3 div (-7)) = { 0 }$

==== Unsigned Storage Classes

given a storage class `uN`, where $N$ is the width in bits, and variables `a : uN[T]`, and `b : uN[T]`
- $a + b = (a + b) mod 2^N$
- $a - b = 2^N - |a - b| mod 2^N$
- $a * b = (a * b) mod 2^N$
- $a div b = (a - (a mod b)) div b$ (i.e. division is always rounded down)
- $~a = (2^N - 1) - a$
- TODO other bitwise ops defined in terms of the above operations
- TODO except xor, maybe?

==== Signed Storage Classes

given a storage class `uN`, where $N$ is the width in bits, and variables `a : sN[T]`, and `b : sN[T]`
- $a + b = (a + b) mod 2^N$
- $a - b = 2^N - |a - b| mod 2^N$
- $a * b = (a * b) mod 2^N$
- $a div b = (a - (a mod b)) div b$
- $~a = (2^N - 1) - a$
- TODO other bitwise ops defined in terms of the above operations
- TODO except xor, maybe?

== Narrowing<sc-narrowing>

A variable's type may be narrowed based on the result of a boolean expression.

```hlt
let x: UInt32 = /* ... */;
let y: &[Char; 0..100] = /* ... */;

if x <= 100 {
 print(y[x]);
}
```

Within this if-statements body, the synthesized type of `x` has been narrowed to `0..100`.


This is achieved by assigning _implications_ to values.
Here, we have `(x <= 100) : 0 | 1`, the value `1` is assigned the implication `x : 0..100`, and the value `0` is assigned `x : 101..0xffffffff`.

A type carrying implications appears in a conditional (at the time of writing, just `if` statements) then the implications of a value, $a$, are applied within a block if the conditional guarantees the expression had the value $a$ before entering the block.

= Disjoint Integer Sets<section-disjoint-integer-sets>

Integer sets are used throughout the type checker. The semantics of our type system (see @sc-scalars) require these sets to implement arithmetic operations in addition to usual set operations like union, intersect, etc. 

Representations of sparse sets in memory is a well-studied topic, with efficient solutions for many use cases. /* TODO: cite: roaring bitmaps, other tree-based repr */
Most of the work we found focuses on storing collections of integers, but performing operations on them isn't well optimized.

To date, we have not found an efficient method of computing the operations laid out in @sc-scalars in the general case. Instead, we've focused on optimizing operations often performed by the programmer, while offering them ways to bypass strict integer checks when required.

Internally, we use 3 set representations, _Stripe Sets_, _Small Sets_ and _Continguous Sets_


== Stripe Sets<sc-stripe-sets>

A _Stripe Set_ is a collection of _Step Ranges_. 

A _Step Range_ is an integer set with minimum element $A$ and maximum element $B$, with some step $S$. 
The set includes all elements $A + n(S)$, for any $n$, where $A + n(S) < n$.
Formally, we define $"STEP"(A, B, S) := { n(S) + A : n in NN, n <= (B - A)\/S }$, where $A, B in ZZ, A <= B$ and $S in ZZ, S >= 1, (B - A mod S) equiv 0$.

#let STEP = "STEP"

#let stripe(..rs) = {
  let elems = rs.pos().map(((a, b, s)) => range(a, b, step: s).map(x => str(x))).flatten().join(", ")
  ${ #elems }$
}


This representation is the most general - it can express any arbitrary set of integers.
But, the in-memory representation can be difficult to manage.

Consider a stripe set: $A = STEP(5, 13, 2) union STEP(20, 26, 3) = #stripe((5, 13, 2), (20, 26, 3))$,
and a stripe set $B = STEP(0, 100, 10) = #stripe((0, 100, 10))$.

How do we add these, in such a way that the result has as few step ranges as possible?
At present, we use one simple algorithm: For each combination action of step ranges $alpha, beta$, take the one with the fewest elements (say $alpha$, for this example). 
For every element $a$ in $alpha$, create a new range $STEP("min"(beta) + a, "max"(beta) + a, "step"(beta))$. Issues quickly arise after several operations, so this representation should be avoided.

== Small Sets<sc-sm-set>

_Small Sets_ is a $1$ KiB uncompressed bit field, with an arbitrary offset.
This is intended to be used for large enumerable values.

A _Small Set_ may be used as the backing store for keyboard scancodes like in SDL2's SDL_keyboard.h, @SDL2IncludeSDL_keyboardh.

== Contiguous Ranges<sc-contiguous-ranges>

Ideally, most ranges we perform arithmetic on should be continuous.
Addition is trivial, and multiplication with a constant just creates a new Step Range.

== Dynamic Represntation

The possible values of any scalar are kept as one of the above types, with a descriminator, this structure is called `DynSet`.
The type checker can construct a new `DynSet` in 2 ways:

1. Using a single value, $a$, (e.g. synthesizing a literal). This creates a contiguous range from $a$ to $a$.
2. Using a type range expression, `a..b`, this creates a contiguous range from `a` to `b`.

From the start of its life as a contiguous range, these dynamic sets can be _upgraded_ to a more suitable representation. For example, after taking the union of two dynamic sets with no overlap, they'll be represented as a stripe set.

= Constraints<sc-contraints>

At the type level, a boolean expression is considered an integer constraint satisfiability problem.
The broad implications are discussed in @sc-narrowing.

Currently, we support binary constraints involving multiplication and addition.
To find these constraints within the abstract syntax tree, we use a similar approach to type checking.
Every node may or may not be represented as a _Constraint Term_.
A constraint term may be a constant; variable; addition or multiplication between a variable and constant; addition or multiplication between two variables; a variable constrained to a set; or a constraint between two variables.

The process for mapping nodes to atomic constraint terms follows: 

- Identifier referencing a mutable variable $->$ _variable_
- Field access to a mutable struct field $->$ _variable_
- Expression with a scalar result $->$ _constant_
- Identifier referencing an immutable variable $->$ _constant_
- Field access to an immutable struct field $->$ _constant_

From there, we build compound terms:

- _variable_ $+,times$ _constant_ $-> $ _unary operation_
- _constant_ $+,times$ _constant_ $-> $ _constant_
- _variable_ $+,times$ _variable_ $-> $ _binary operation_
- _variable_ $<,>,<=,>=,!=,=$ _variable_ $-> $ _binary constraint_
- _variable_ $<,>,<=,>=,!=m=$ _constant_ $-> $ _unary constraint_
- _variable_ $<,>,<=,>=,!=m=$ _unary operation_ $-> $ _binary constraint_


A collection of unary and binary constraints, combined with the logical and operator (`&&`) form a constraint set. We reduce each of the variable's values to satisfy the constraint, or, warn the user that the consideration will never be satisfied if this fails. Because we only handle expressions involving two mutable variables, expressions that do not meet this criterion are ignored.

== Solving Constraints

We solve constraints with a naive constraint propagation algorithm, based on the algorithms described in _Foundations of Artificial Intelligence_, Chapter 3, @bessiereChapter3Constraint2006. A constraint set is a collection of variables and constraints on those variables. All variables begin with some unary constraint, by default this is that they must be a subset of their current synthesized type. From there, we iterate over every unsatisfied constraint, each constraint "propagates", returning that either it has been satisfied, it cannot be satisfied, or a mutation
to some variable that is required for it to be satisfied (although it's not guaranteed that it will be satisfied immediately after the mutation is made).
This scheme was originally inspired by Zhou's _Action Rules_ language (@zhouProgrammingFinitedomainConstraint2006).
The search is depth-first, if a constraint is found to be unsatisfiable, we undo the last mutation and move up the tree.
The first set of mutations that satisfy all constraints is used to produce the final collection of values.

= Code Generation

Similar to type checking, code generation works by folding the abstract syntax tree.
Each node writes to a buffer of assembly instructions, provided by the parent node.
They return a collection of _Slots_ which contain the value of their computation.
A Slot may be a register, a pointer (itself a slot) and an associated offset, or a 16-bit immediate. No optimizations are performed, and the generated code is generally inefficient, even compared to other compilers when they skip the optimization step.