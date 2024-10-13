#import "@preview/classic-jmlr:0.4.0": jmlr
#import "@preview/wrap-it:0.1.0": wrap-content
#import "@preview/fletcher:0.5.1" as fletcher: diagram, node, edge


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
  abstract: include "./abstract.typ",
  keywords: ("programming language",),
  bibliography: bibliography("main.bib"),
  appendix: include "appendix.typ",
)


= Introduction

Memory safety in systems programming languages /* TODO: define this term */ has garnered a lot of attention in the last several years. /* TODO: do I need to cite this claim? */
A compiler that enforces strict rules on object's lifetime and mutability is helpful in large projects, especially when security is a top concern. Checking these properties at compile time allows the compiler to omit parts of its runtime, like a garbage collector, while providing similar gaurentees.

These innovations in language design fail to directly address a class of problems where direct memory manipulation is essential. These problems force the programmer to fully disable the compiler's checks, or encourage awkward solutions which trade clarity for small guarentees. /* TODO: weak claim, I already hyped these guarentees, why do I say "weak" all of a sudden? */

Howlite aims to address these problems. Howlite is not a language to write a web server, it is not for writing applications, it isn't even a language for writing programming languages. It is a language for writing a single module for a very specific data structure, wrapped in a python library. It is a language for writing a boot loaded, or the entrypoint to a kernel. The compiler does not impose strict requirements on how the programmer manages memory, or accesses data. Instead, the type systems gives a rich set of tools, allowing one to set their own constraints.

// #show raw: set text(size: 1em)

= Syntax

#let fig = [
  #show <eg-syn-bounded-add>: set text(size: 10pt)

  #figure(
    caption: [Addition without Overflow],
    rect(
          pad(x: 4pt, y: 4pt)[
          ```
          func boundedAdd(a: u32, b: u32): u32 {
            if U32_MAX - a > b {
              U32_MAX
            } else {
              a + b
            }
          }
          ```
        ]
      )
    )<eg-syn-bounded-add>
]

#wrap-content(fig, align: left)[
  Howlite's syntax prioritizes familiarity, ease of parsing, and clarity. The syntax should be familiar, someone unfamiliar with the language should be able to immediately grasp the programmer's intent, event if they do not understand every line. In a similar vein, the programmer should be guided towards writing code that is easily legible by others. We approach this issue by providing language constructs that clearly express intent. For example, flow control constructs, like if statements may have a value. This allows the programmer to clearly show a variable's value is the result of some condition. In order to make tooling easier to write, we prioritize createing an unambigous grammar, with no constructs that require unbounded look-ahead.
]

== Familiarity

Howlite code should be recognizable to C programmers. For this reason, we use curly braces ("{" and "}") to denote blocks of code. We use familiar imperative /* TODO: define imperative */ keywords: "if", "else", and "while", and mathmatical expressions follow typical infix notation. Howlite differs from C in that it requires a sigil character or keyword before beginning a new construct. Types do not lead in variable assignments or functions. Instead we use the "let" or "func" keywords, respectively. This simplifies parsing, since we know what type of statement or expression will follow, similarly, type ascriptions are always prefixed with `:`. These keywords and symbols were decided by surveying popular languages during design. For example, "let", and `:` come from TypeScript, while "func" is a keyword in Go.

== Clarity

*TODO*



= Type Checking 


Howlite's implements a simple bi-directional type checker [@dunfield_bidirectional_2020]. Every node in the AST is given a type. An AST node's type is typically derived from it's children's types, through a process called _synthesis_, we call these types _synthesized types_. Many constructs in the language must be ascribed types by the programmer: variables declared with "`let`", function parameters, and return values. Types which are declared explicitly are called _assumed_ types.


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

=== Typechecking an AST

To better illustrate this process, we'll walk through synthesizing a tree.

  ```
  func average(x : 0..10, y : 0..10, z : 0..10) : 0..10 {
    (x + y + z) / 3
  }
  ```
The function parameters: `x`, `y`, and `z` have each been given the assumed types `UInt32`. An assumed type is analogous to the the statement "no matter the value of `x`,  we can always assume it is a `UInt32`". The function's assumed return type is `UInt32`. This allows any caller to treat the expression `average(a, b, c)` as a `UInt32`, even if the operations performed by the function are unknown. An assumed type is a promise; it allows the references to entity to _assume_ the type of that entity, without knowing anything else about it.

To illustrate how these assumed types interact with synthesized types, we'll manually type check the function.

#let colors = (
  l1: red.lighten(50%),
  l2: green.lighten(75%),
  l3: blue.lighten(75%),
  l4: yellow.lighten(75%)
)

// reference a diagram node inline
#let noderef(label, color) = if label.text.len() == 1 { box(height: 16pt, baseline: 4pt, outset: -0.5pt, width: 16pt, align(center + horizon, circle(fill: color, label))) } else { box(height: 16pt, fill:color, baseline: 3.5pt, outset: -0.5pt, inset: 5pt, align(center + horizon, label)) }

#let full-tree = [#figure(diagram({
  node((0, 0), `/`, fill: colors.l1, name: <div>)
  edge("-|>")
  node((-.6, 1), `+`, fill: colors.l2, name: <add1>)
  edge("-|>")
  node((-1.2, 2), `+`, fill: colors.l3, name: <add2>)
  edge("-|>")
  node((-1.8, 3), `x`, fill: colors.l4)
  edge(<add2>, (-.6, 3), "-|>")
  node((-.6, 3), `y`, fill: colors.l4)

  edge(<add1>, (0, 2), "-|>")
  node((0, 2), `z`, fill: colors.l3)


  edge(<div>, (.6, 1), "-|>")
  node((.6, 1), `3`, fill: colors.l2)
  
}, node-outset: 3pt, spacing: 1.5em), caption: "AST")<ast>]

#wrap-content(pad(right: 8pt, bottom: 8pt, full-tree))[
  The funtion body, `(x + y + z) / 3`, has the syntax tree seen in @ast.
The type checker works bottom-up, left-to-right. So, we begin with the leaves of the tree: #noderef(`x`, colors.l4), and #noderef(`y`, colors.l4). Identifier AST node's synthesized type is the assumed type of the symbol they include. So #noderef(`x`, colors.l4) is synthesized to type `0..10` (the assumed type of `x`), and #noderef(`y`, colors.l4) is synthesized to type `0..10` (the assumed type of `y`). 

This information is added to the tree, and we reference it synthesize #noderef(`+`, colors.l3). An operator node's synthesized type is constructed by applying the given operation to the synthesized types of each operand. Types may be constructed using arithmetic operations, this process will be defined more formally in @scalars. For now, take for granted that `0..10 + 0..10 : 0..20`. 
]

#stack(dir: ltr, spacing: 1em,
  diagram({
    node((0, 0), `+`, fill: colors.l3, name: <add2>)
    edge("-|>")
    node((-.8, 1), `x : 0..10`, fill: colors.l4)
    edge(<add2>, (.8, 1), "-|>")
    node((.8, 1), `y : 0..10`, fill: colors.l4)  
  }, node-outset: 3pt, spacing: 1.5em),
  $-->$,
  diagram({
    node((0, 0), `+ : 0..20 `, fill: colors.l3, name: <add2>)
    edge("-|>")
    node((-.66, 1), `x : 0..10`, fill: colors.l4)
    edge(<add2>, (.66, 1), "-|>")
    node((.66, 1), `y : 0..10`, fill: colors.l4)
  }, node-outset: 3pt, spacing: 1.5em)
)
Now, we move up the tree, to synthesize the right hand side of #noderef(`+`, colors.l2), then finally #noderef(`+`, colors.l2) itself.

#stack(dir: ltr, spacing: .5em,
  diagram({
    node((0, 0), `+`, fill: colors.l2, name: <add1>)
    edge("-|>")
    node((-.8, 1), `+ : 0..20`, fill: colors.l3)
    edge(<add1>, (.8, 1), "-|>")
    node((.8, 1), `z`, fill: colors.l3)  
  }, node-outset: 3pt, spacing: 1.5em),
  $-->_1$,
  diagram({
    node((0, 0), `+`, fill: colors.l2, name: <add1>)
    edge("-|>")
    node((-.8, 1), `+ : 0..20`, fill: colors.l3)
    edge(<add1>, (.8, 1), "-|>")
    node((.8, 1), `z : 0..10`, fill: colors.l3)  
  }, node-outset: 3pt, spacing: 1.5em),
  $-->_2$,
  diagram({
    node((0, 0), `+ : 0..30`, fill: colors.l2, name: <add1>)
    edge("-|>")
    node((-.6, 1), `+ : 0..20`, fill: colors.l3)
    edge(<add1>, (.8, 1), "-|>")
    node((.6, 1), `z : 0..10`, fill: colors.l3)  
  }, node-outset: 3pt, spacing: 1.5em),
)

In (1) we synthesize the node's type from the assumed type of `z`. In (2) we used this information, and the type of #noderef(`+`, colors.l3) to synthesize a type for #noderef(`+`, colors.l2).

Finally, we again move up the tree, now to #noderef(`/`, colors.l1).

#stack(dir: ltr, spacing: .5em,
  diagram({
    node((0, 0), `/`, fill: colors.l1, name: <add1>)
    edge("-|>")
    node((-.8, 1), `+ : 0..30`, fill: colors.l2)
    edge(<add1>, (.8, 1), "-|>")
    node((.8, 1), `3`, fill: colors.l2)  
  }, node-outset: 3pt, spacing: 1.5em),
  $-->_1$,
  diagram({
    node((0, 0), `/`, fill: colors.l1, name: <add1>)
    edge("-|>")
    node((-.8, 1), `+ : 0..30`, fill: colors.l2)
    edge(<add1>, (.8, 1), "-|>")
    node((.8, 1), `3 : 3`, fill: colors.l2)  
  }, node-outset: 3pt, spacing: 1.5em),
  $-->_2$,
  diagram({
    node((0, 0), `/ : 0..10`, fill: colors.l1, name: <add1>)
    edge("-|>")
    node((-.8, 1), `+ : 0..30`, fill: colors.l2)
    edge(<add1>, (.8, 1), "-|>")
    node((.8, 1), `3 : 3`, fill: colors.l2)  
  }, node-outset: 3pt, spacing: 1.5em),
)

Due to the the functions return value, the assumed type of the body is `0..10`. 
Function body's type is synthesized based on the possible return values. 
So the synthesized type of this function's body is the the type of #noderef(`/`, colors.l1).

Type checking is the process of comparing assumed and synthesized types. 
If a synthesized is not a subset of the assumed type, then a type error is attached to that node. 



== Scalars <scalars>

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

Scalar types belong to a _storage class_ that identifies how they are encoded in memory. Storage classes are organized by size, whether or not they include a sign bit. The signed storage classes are `s8`, `s16`, `s32`, `s64`, and the unsigned are `u8`,`u16`,`u32`,`u64`. Going forward, we will identify the storage class of a scalar `T` using the notation `u32<{T}>`.

The storage class of a number influences how arithmetic and bitwise operations behave on the inner type.

==== Unsigned Storage Classes

given a storage class `uN`, where $N$ is the width in bits, and variables `a : uN<{T}>`, and `b : uN<{T}>`
- $a + b = (a + b) mod 2^N$
- $a - b = 2^N - |a - b| mod 2^N$
- $a * b = (a * b) mod 2^N$
- $a / b = (a - (a mod b)) / b$ (i.e. division is always rounded down)
- $~a = (2^N - 1) - a$
- TODO other bitwise ops defined in terms of the above operations
- TODO except xor, maybe?

==== Signed Storage Classes

given a storage class `uN`, where $N$ is the width in bits, and variables `a : sN<{T}>`, and `b : sN<{T}>`
- $a + b = (a + b) mod 2^N$
- $a - b = 2^N - |a - b| mod 2^N$
- $a * b = (a * b) mod 2^N$
- $a / b = (a - (a mod b)) / b$ (i.e. division is always rounded down)
- $~a = (2^N - 1) - a$
- TODO other bitwise ops defined in terms of the above operations
- TODO except xor, maybe?