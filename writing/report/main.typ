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
   email: "irhsheadeh@smcm.edu"),
)

#show: jmlr.with(
  title: [A Low Level Language with Precise Integer Types],
  authors: (authors, affls),
  abstract: include "./snippets/abstract.typ",
  keywords: ("programming language",),
  bibliography: bibliography("main.bib"),
  appendix: include "snippets/appendix.typ",
)


// Audience expectations: some familiarity with programming languages, possibly some familiarity with set theory

= Programming Language Concepts and Motivation

_Programming Language_ is a broad term, generally, it's a text-based format for expression computation.
Although most programs are written by software engineers, and most programming languages are designed with software engineers in mind, this by no means makes the a homogenous group.
After a program is written it's read by a machine (the compiler or interpret) and people - possible just the original author, or others they collaborate with.
To satisfy both audiences it must be clear two ways: formally, so the machine can produce consistent and accurate results, and legible, the original author's intent should be clear.

How we make a programming language clear depends entirely on what it's meant to accomplish, to demonstrate, consider _Hedy_, @hedy, and _Go_, @go-github.

#align(center, grid(
    include "examples/hedy.typ",
    include "examples/go.typ",
    columns: 2,
    gutter: 3em,
  )
)

== Hedy

Hedy @hedy is a programming language for teaching programming.


The language avoids symbols, instead using keywords, which are generally easier for students to remember. With so much of the language being textual, Hedy is fully translated to a large set of languages - 47 at the time of writing. Both Hedy programs above do the same, but they're keywords are in different languages. Hedy also allows programmers to see and hear the results of their work: it has easily accessible functionality for playing music and drawing graphics. Those features are typically implemented as libraries, for most programming languages, since they have a relatively narrow application. But, it takes a lot of general knowledge to have the computer _do_ anything interesting; when first learning, it's helpful to be able to make the computer *do* something, not just report results in the terminal.

== Go


Go was an answer to problems with the software infrastructure at Google (@pike_GoAtGoogle).
It's designed to be used in large, long-lived software projects. There’s a focus on clear syntax and semantics: no matter who wrote the code, what it does, and how it does it should be clear to any programmer. It also comes bundled with tools to help keep programs up to date and consistent.
Unused variables and imports are disallowed. Although it supports first class functions, it's largely imperative. Programmers are forced to deal with the inherent complexity of things like string encoding up front, as seen in @ex-go.


== Howlite's Purpose

#wrapped-figure(
  left: include "examples/howlite-general.typ",
  right: [

Howlite is an expiremental language for writing programs that necessitate little abstraction over they machines they control. The project's goal is to answer the question, _How can we create an expressive type system without limiting a programmer's control of the hardware?_

])

= The Programming Language

Memory safety in systems programming languages /* TODO: define this term */ has garnered a lot of attention in the last several years. /* TODO: do I need to cite this claim? */
A compiler that enforces strict rules on object's lifetime and mutability is helpful in large projects, especially when security is a top concern. Checking these properties at compile time allows the compiler to omit parts of its runtime, like a garbage collector, while providing similar gaurentees.

These innovations in language design fail to directly address a class of problems where direct memory manipulation is essential. These problems force the programmer to fully disable the compiler's checks, or encourage awkward solutions which trade clarity for small guarentees. /* TODO: weak claim, I already hyped these guarentees, why do I say "weak" all of a sudden? */

Howlite aims to address these problems. Howlite is not a language to write a web server, it is not for writing applications, it isn't even a language for writing programming languages. It is a language for writing a single module for a very specific data structure, wrapped in a python library. It is a language for writing a boot loaded, or the entrypoint to a kernel. The compiler does not impose strict requirements on how the programmer manages memory, or accesses data. Instead, the type systems gives a rich set of tools, allowing one to set their own constraints.


= Syntax Design

#wrapped-figure(
  right: [#include "./examples/syntax-overview.typ"],
  left: [
    Howlite's syntax prioritizes familiarity, ease of parsing, and clarity. The syntax should be immediately familiar to anyone who knows another C-like language. The grammar is context free, so it can easily be expressed using a parser generator. The language's syntax should give programmer's the tools to express their intent.
  ],
  under: [
      For example, flow control constructs, like if statements may have a value. This allows the programmer to clearly show a variable's value is the result of some condition. In order to make tooling easier to write, we prioritize createing an unambigous grammar, with no constructs that require unbounded look-ahead.
  ])

== Familiarity

Howlite code should be recognizable to C programmers. For this reason, we use curly braces ("{" and "}") to denote blocks of code. We use familiar imperative /* TODO: define imperative */ keywords: "if", "else", and "while", and mathmatical expressions follow typical infix notation. Howlite differs from C in that it requires a sigil character or keyword before beginning a new construct. Types do not lead in variable assignments or functions. Instead we use the "let" or "func" keywords, respectively. This simplifies parsing, since we know what type of statement or expression will follow, similarly, type ascriptions are always prefixed with `:`. These keywords and symbols were decided by surveying popular languages during design. For example, "let", and `:` come from TypeScript, while "func" is a keyword in Go.

== Ease of Parsing

A small, easily parsed grammar is valuable because it makes implementing tooling easier. Anything from simple syntax highlighting in _Emacs_ to an auto-formatter or linter dramatically easier to implement when parsing the language isn't a significant hurdle.

Howlite's syntax is expressable in an LR grammar. Consequently, the grammar is unambiguous, furthermore, the grammar aims to reduce look ahead as much as possible.

== Clarity

Here, we use clarity to mean the ease of understanding a program's behavior.
If a program is clear, then the author's original intent should be easily understood by someone familiar with the language. The author of a program is responsible for making their intent clear; the syntax should guide their choices, and give them the tools to express their intent.

We optimize clarity by keeping tokens consistent, for example colon (`:`) is almost always a way to give _something_ a type, whether that thing is an expression, variable, or a field of a data structure. However, we don't sacrifice familiarity for consistency. Languages like C, C++, Java, Go, and more use curly braces for both structure declarations and statement blocks, so we follow suite.

Giving programmers tools to express their intent extends beyond syntax, but the features a language's syntax emphasizes plays an important role in guiding the programmer.
We chose to make constructing types easy.

= Type Checking<sc-type-checking>

We optimize clarity by keeping tokens consistent, for example colon (`:`) is almost always a way to give _something_ a type, whether that thing is an expression, variable, or a field of a data structure. However, we don't sacrifice familiarity for consistency. Languages like C, C++, Java, Go, and more use curly braces for both structure declarations and statement blocks, so we follow suite.

Howlite's implements a simple bi-directional type checker [@dunfieldBidirectionalTyping2020]. Every node in the AST is given a type. An AST node's type is typically derived from it's children's types, through a process called _synthesis_, we call these types _synthesized types_. Many constructs in the language must be ascribed types by the programmer: variables declared with "`let`", function parameters, and return values. Types which are declared explicitly are called _assumed_ types.


#let fig1 = [#figure(
    rect(
      pad(x: 8pt, y: 8pt)[
      ```
      let a: Uint32 = 1;
      ```
    ]
  ), caption: [Simple Let statement])<eg-ty-let>]

#wrap-content(fig1, align: left)[
  Here, `Uint32` is the assumed type of `x`. Where ever `x` is referenced, we can consider it of type `Uint32`. The literal `1` has no assumed type. Instead, we synthesize a type for `1` by following a set of rules. For literals, this rule is simple: _for a literal scalar $N$ the synthesized type is ${ N }$_. As expressions grow, synthesizing types becomes more complicated.
]

=== Type Checking an AST

To better illustrate this process, we'll walk through synthesizing a tree.

  ```
  func average(x : 0..10, y : 0..10, z : 0..10) : 0..10 {
    (x + y + z) / 3
  }
  ```
The function parameters: `x`, `y`, and `z` have each been given the assumed types `UInt32`. An assumed type is analogous to the the statement "no matter the value of `x`,  we can always assume it is a `UInt32`". The function's assumed return type is `UInt32`. This allows any caller to treat the expression `average(a, b, c)` as a `UInt32`, even if the operations performed by the function are unknown. An assumed type is a promise; it allows the references to entity to _assume_ the type of that entity, without knowing anything else about it.

To illustrate how these assumed types interact with synthesized types, we'll manually type check the function.

#import "examples/typechecking/colors.typ": noderef-1, noderef-2, noderef-3, noderef-4

#wrap-content(
  pad(right: 8pt, bottom: 8pt, include "./examples/typechecking/full-tree.typ")
)[
  The funtion body, `(x + y + z) / 3`, has the syntax tree seen in @ast.
The type checker works bottom-up, left-to-right. So, we begin with the leaves of the tree: #noderef-4(`x`), and #noderef-4(`y`). Identifier AST node's synthesized type is the assumed type of the symbol they include. So #noderef-4(`x`) is synthesized to type `0..10` (the assumed type of `x`), and #noderef-4(`y`) is synthesized to type `0..10` (the assumed type of `y`).

This information is added to the tree, and we reference it synthesize #noderef-3(`+`). An operator node's synthesized type is constructed by applying the given operation to the synthesized types of each operand. Types may be constructed using arithmetic operations, this process will be defined more formally in @section-scalars. For now, take for granted that `0..10 + 0..10 : 0..20`.
]

#include "examples/typechecking/reduce-1.typ"
Now, we move up the tree, to synthesize the right hand side of #noderef-2(`+`), then finally #noderef-2(`+`) itself.


#include "examples/typechecking/reduce-2.typ"
In (1) we synthesize the node's type from the assumed type of `z`. In (2) we used this information, and the type of #noderef-3(`+`) to synthesize a type for #noderef-3(`+`).

Finally, we again move up the tree, now to #noderef-1(`/`).

#include "examples/typechecking/reduce-3.typ"
Due to the the functions return value, the assumed type of the body is `0..10`.
Function body's type is synthesized based on the possible return values.
So the synthesized type of this function's body is the the type of #noderef-1(`/`).

Type checking is the process of comparing assumed and synthesized types.
If a synthesized is not a subset of the assumed type, then a type error is attached to that node.



== Scalars<section-scalars>

There is a single scalar type in Howlite, this simplifies the type checking by condensing many cases into a single, generic case. There are no distinct enumerable types, true boolean types, or even a unit type in the language. Instead of distinct types, we have the scalar type "Integer" (floating point number are out of scope). A scalar may be any set of Integers.

=== Synthesis of Scalars

As seen above, a scalar may be systhensized from a single value, for example the type of $-5$ is ${ -5 }$. We can also construct new scalars using arithmetic operations:

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

=== Storage Classes

Scalar types belong to a _storage class_ that identifies how they are encoded in memory. Storage classes are organized by size, whether or not they include a sign bit. The signed storage classes are `s8`, `s16`, `s32`, `s64`, and the unsigned are `u8`,`u16`,`u32`,`u64`. Going forward, we will identify the storage class of a scalar `T` using the notation `u32[T]`.

The storage class of a number influences how arithmetic and bitwise operations behave on the inner type.

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
Here, we have `(x <= 100) : 0 | 1`, the value `1` is assinged the implication `x : 0..100`, and the value `0` is assigned `x : 101..0xffffffff`.

A type carrying implications appears in a conditional (at the time of writing, just `if` statements) then the implications of a value, $a$, are applied within a block if the conditional gaurentees the expression had the value $a$ before entering the block.

= Disjoint Integer Sets<section-disjoint-integer-sets>

Integer sets are used throughout the type checker. The semantics of our type system (see @section-scalars) require these sets implement arithmetic operations in addition to usual set operations like union, intersect, etc. 

Representations of sparse sets in memory is a well studied topic, with efficient solutions for many use cases. /* TODO: cite: roaring bitmaps, other tree-based repr */
Most of the work we found focuses on storing collections of integers, but performing operations on them isn't well optimized.

To date, we have not found an efficient method of computing the operations laid out in @section-scalars in the general case. Instead, we've focused on optimizing operations often performed by the programmer, while offering them ways to bypass strict integer checks when required.

Internally, we use 3 set representations, _Stripe Sets_, _Small Sets_ and _Continguous Sets_


== Stripe Sets<sc-stripe-sets>

A _Stripe Set_ is a collection of _Step Ranges_. 

A _Step Range_ is an integer set with minimum element $A$ and maximum element $B$, with a some step $S$. 
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
At present we use one simple argorithm: For each combinationation of step ranges $alpha, beta$, take the one with the fewest elements (say $alpha$, for this example). 
For every element $a$ in $alpha$, create a new range $STEP("min"(beta) + a, "max"(beta) + a, "step"(beta))$. Issues quickly arise after a number of operations, so this representation should be avoided.

== Small Sets<sc-sm-set>

_Small Sets_ is a $1$ KiB uncompressed bit field, with an arbitrary offset.
This is intended to be used for large enumerable values.

A _Small Set_ may be used as the backing store for a keyboard scancodes like in SDL2's SDL_keyboard.h, @SDL2IncludeSDL_keyboardh.

== Contiguous Ranges<sc-contiguous-ranges>

Ideally, most ranges we perform arithmetic on should be continuous.
Addition is trivial, and multiplication with a constant just creates a new Step Range.

== Dynamic Represntation

The possible values of any scalar is kept as one of the above types, with a descriminator, this structure is called `DynSet`.
The type checker can construct a new `DynSet` in 2 ways:

1. Using a single value, $a$, (e.g. synthesizing a literal). This creates a contiguous range from $a$ to $a$.
2. Using a type range expression, `a..b`, this creates a contiguous range from `a` to `b`.

From the start of its life as a contiguous range, these dynamic sets can be _upgraded_ to a more suitable representation. For example, after taking the union of two dynamic sets with no overlap, they'll be represented as a stripe set.

= Constraints<sc-contraints>

At the type level, a boolean expression is considered an integer constraint satisifiability problem.
The broad implications are discussed in @sc-narrowing.

Currently we support binary constraints involving multiplcation and addition.
To find these constraints within the abstract syntax tree, we use a similar approach to type checking.
Every node may or may not be represented as a _Constraint Term_.
A constraint term may be a constant, addition or multiplication between a variable and constant, addition or multiplcation between two variables, a variable constrainted to a set, or a constraint between two variables. 

= Code Generation

