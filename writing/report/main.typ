#import "@preview/classic-jmlr:0.4.0": jmlr
#import "@preview/wrap-it:0.1.0": wrap-content
#import "@preview/fletcher:0.5.1" as fletcher: diagram, node, edge
#import "./util.typ": *
#import "./templates/example.typ": code-sample


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
To satisfy both audiences, it must be clear in two ways: first, it needs to be unambiguous so the machine can produce consistent and accurate results; second, it must be expressive, meaning the author's intent should be apparent. To give context about what choices language designers make we introduce _Hedy_, and _Go_.

== Hedy

#wrapped-figure(
    left: include "examples/hedy.typ",
    right: [
      Hedy [@hedy] is a programming language for teaching programming.
      The language avoids symbols, instead using keywords, which are generally easier for students to remember. With so much of the language being textual, Hedy is fully translated to a large set of languages, 47 at the time of writing. The programs in @ex-hedy-english and @ex-hedy-arabic both print "Hello!" and ask the user their name, despite their keywords being in different languages. Hedy also allows programmers to see and hear the results of their work: it has easily accessible functionality for playing music and drawing graphics. Those features are typically implemented as libraries for most programming languages,
    ],
    under: [
      since they have a relatively narrow application. But, by putting these features in the core language Hedy allows fairly inexpierenced programmers access the machines more complex functionality, at the cost of performance and fine-grained control.
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

Howlite is an expiremental language for writing programs that necessitate little abstraction over the machines they control. The project's goal is to answer the question, _How can we create an expressive type system without limiting a programmer's control of the hardware?_

])

= The Programming Language<sc-howlite-intro>

Memory safety in systems programming languages has garnered a lot of attention in the last several years. 
A compiler that enforces strict rules on an object's lifetime and mutability is helpful in large projects, especially when security is a top concern. Checking these properties at compile time allows the compiler to omit parts of its runtime, like garbage collection, while providing similar guarantees.

These innovations in language design fail to directly address a class of problems where direct memory manipulation is essential. These problems force the programmer to fully disable the compiler's checks, or encourage awkward solutions that trade clarity for small guarantees. 

Howlite aims to address these problems. Howlite is not a language to write a web server, it is not for writing applications, it isn't even a language for writing programming languages. It is a language for writing a single module for a specific data structure, wrapped in a Python library. It is a language for writing a bootloader or the entry point to a kernel. The compiler does not impose strict requirements on how the programmer manages memory or accesses data. Instead, the type systems provide a rich set of tools, that enable programmers to precisely describe how data is transformed.


#wrapped-figure(
  right: include "examples/boolean.typ",
  left: [
== Overview

The most notable feature of Howlite is the type system. The type system is structural and closely tracks the value of integers. For example, you can declare types that only allow the values `1`, `2`, or `5`, most programming languages only offer a few fixed-sized integer types. Types are compared based on their structural compatibility, not by name. 
],
  under: [
    So, if type $T$'s underlying binary representation is a superset of a type $U$'s underlying binary representation, then $T$ is assignable to $U$.  For example, the data structure `{ x: int, y: int, z: int }` is compatible with
    the type `{ x: int, y: int }` (for more detail see @sc-polymorphism).  
  ]
)


To better understand the language, this section will walk through the process of defining a function to get the index of a character from an ASCII string.

#wrapped-figure(
  left: include "examples/index_of/typedefs.typ",
  right: [
    First, we define a character as any number between `0` and `127` (i.e. 7-bit ASCII characters).
    Next is the definition of a standard 32-bit integer, which is used to index the array.
    Finally, we define NatI32, an unsigned 31-bit integer.
  ],
  under: [
      We'll use this type to represent the index, which can't be negative, the sign bit is reserved to signal that the character wasn't found.
  ]
)


Now, we move on to the function signature. The syntax will be familiar to Go programmers, with a few small changes.
#include "examples/index_of/signature.typ"


This function is generic, the `[LenT: NatI32]` section indicates that for any subset of the positive, signed, 32-bit integers, there is an instance of `index_of`.
Whatever that type is, it is referred to as `LenT` within the context of this function.

Moving on to the parameter list, notice the type of `str` is `&[char; LenT]`.
This `&[...]` is a special type called a _slice_ (also known as a fat pointer).
A slice is simply a pointer and length pair; practically it functions like an array.
Slice types are common: they're primitives in Rust, Go, and Zig.
Although it's not a primitive type, the C++ STL's `std::span` is a similar data structure.
What sets our slice type apart is that the type of the length can be set.
For example, say we take a slice of some ASCII string, from index 3 to 10, the result would have the type `&[char; 7]`.

By using a generic parameter, `LenT`, then giving `str` the type `&[char; LenT]`.
We can be certain this function only works on a string of length less than or equal to 0x7fffffff.
Since it's impossible to find a character outside of those bounds, we know the return type can't exceed the maximum value of `LenT`, if no character is found then we return `-1`.


#wrapped-figure(
  right: include "examples/index_of/body.typ",
  left: [
   Finally, the body of this function likely looks familiar to C programmers, with some minor syntactic changes. The most noticeable changes are inherited from Rust [@rust]: Variables are declared with `let`, all expressions (including `if` statements and blocks) have values. The value of a block is equal to the value of the last line in the block if that line omits a semi-colon (`;`), or `unit` otherwise.

    Some care must be taken to make sure we satisfy the return type.
    The compiler must be certain `i` is always a subset of `0..Max[LenT]`, even though `i`'s declared type (`UInt32`) certainly exceeds `LenT`.
    The type check does this by analyzing the condition, "`while i < str.len`", and narrows `i`'s type from `UInt32` to `0..Max[LenT]-1`.
  ],
  under: [
    This means within the body of that loop, `i` can be used as if it had the type `0..Max[LenT]-1`.
    Arithmetic will modify this type: after running `i = i + 1`, `i`'s narrow type has changed to `1..Max[LenT]`.
    If we changed the code to check some other condition, for example, "`chr < str.len`", this wouldn't compile.
  ]
)


= Syntax Design<sc-syntax-design>

#wrapped-figure(
  right: [#include "./examples/syntax-overview.typ"],
  left: [
    Howlite's syntax prioritizes familiarity, ease of parsing, and clarity. The syntax should be immediately familiar to anyone who knows another C-like language. The grammar is context-free, so it can easily be expressed using a parser generator. The language should also clearly reflect exactly what the machine will do when executing the compiled program.
  ])

== Familiarity

Howlite code should be recognizable to C programmers. For this reason, we use curly braces ("{" and "}") to denote blocks of code. We use familiar imperative keywords: "if", "else", and "while", and mathmatical expressions follow typical infix notation. Howlite differs from C in that it requires a sigil character or keyword before beginning a new construct. Types do not lead in variable assignments for functions. Instead we use the "let" or "func" keywords. These keywords and symbols were decided by picking from popular languages during design. For example, "let", and `:` come from TypeScript, while "func" is a keyword in Go.

== Ease of Parsing

A small, easily parsed grammar is valuable because it makes implementing tooling easier. Anything from simple syntax highlighting in _Emacs_ to an auto-formatter or linter is dramatically easier to implement when parsing the language isn't a significant hurdle.

Howlite's syntax is expressable in an LR grammar. Consequently, the grammar is unambiguous. While writing the grammar, we aimed to reduce look ahead as much as possible. For example, functions' type parameters are written `index_of[:u32](...)`, which disambiguates the use of `[...]` from array access.

== Clarity

We use clarity to mean the ease of understanding a program's behavior.
If a program is clear, then the author's original intent should be easily understood by someone familiar with the language. Ultimately, the author of a program is responsible for making their intent clear; the syntax should guide their choices, and give them the tools to express their intent.

We optimize clarity by keeping tokens consistent, for example, colon (`:`) is almost always a way to give _something_ a type, whether that thing is an expression, variable, or a field of a data structure. However, we don't sacrifice familiarity for consistency. Languages like C, C++, Java, Go, and more use curly braces for both structure declarations and statement blocks, so we follow suit.

Being a low-level language, we want to emphasize precisely what the machine is doing.
Howlite programs are written in an imperative style, we expect the programmer to use mutable state, but discourage it when unnecessary by making it opt-in via the `mut` keyword. We also omit short-hand syntax or functions for functional operations, like transforming the content of an array. While these operations are convenient, they can paper over important details like memory allocation. 

For example, flow control constructs like if statements may have a value. This allows the programmer to clearly show a variable's value is the result of some condition.

= Bare Metal Polymorphism<sc-polymorphism>

As outlined in @sc-howlite-intro Howlite supports two kinds of polymorphism: subtype and parametric polymorphism.
To maintain goal of staying close to the hardware, there are several limitations on both.

First, parametric polymorhpism operates entirely at the type level.
It has no bearing on the generated code.
The in-memory representation of the type `type Vec2[T: Uint32] = { x: T, y: T }` is identical to that of `{ x: Uint32, y: Uint32 }`.
This feature was inspired by the research language Cycle @cyclone_types.
The key difference is that in Howlite, you can define a type parameter to be a subset of any type, not just pointer-sized types or smaller.
This limited form of polymorphism is mostly useful for giving strong typing to pointer types and integer types. Ideally, it allows the programmer to avoid using untyped points (like C's `void*`) when implementing datastructures like dynamic arrays, or passing context for a callback function.

Subtype polymorhpism is a consequence of structural typing.
Types are not compared by name, but instead their contents: so, $50$ is assignable to the type `1..100`, because it is a member of that type. This extends to arrays: `[Char; 10]` is assignable to `[Char; 5]`, becuase it has at least 5 elements. Similarly, a data structure `{ a: int, b: int, c: int, d: int }` can be assigned to `{b: int, c: int}`.

In practice, this is achieve in two ways: first, if the superset data structure is passed-by-reference, or a reference to a superset datastructure is assigned to a reference to a subset data structure, then the underlying pointer is adjusted to align the fields. In the above example the reference would be shifted up by `sizeof(int)` when assigned to `{b: int, c: int}`. This has little to no runtime cost - it usually means adding an immediate offset to future load instructions. If the assignment is not using a reference then only the relevant fields are copied. To make sure this system is sound, we impose the follow restrictions on both arrays and structures:
1. The superset type must be at least as big as the subset type
2. Every field in the subset type must have the same size as the same field in the superset
3. Every field in the subset type must have the same offset as the same field in the superset.
4. Every field in the superset type must be assignable to the equivalent field in the subset type.



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

However, if overflow is known to be harmful then it can be explicitly forbidden. For example. Suppose we're reading a 64-bit ELF file, a common executable file format on Unix-like systems, we can read the address and size of a particular section from the $4^"th"$ and $5^"th"$ words of its _Section Heder_ entry @SystemApplicationBinary2013a:

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

A scalar is constructed by arithmetic operations, character literals or integer literals.
We will use Howlite's own type construction syntax going forward: the given an expression `e` and a type `T`, the expression `e : T` asserts `e` constructs the type `T`.

A literal scalar can be constructed from a character literal, like `'A'`, `'\n'`, or `'🤯'`. 
The type constructed is a single value, equal to their Unicode codepoint. So, #cons_c("A"), #cons_c("\\n", v: "\n"), #cons_c("🤯").

Literals can also be constructed from unsigned integers: `3 : 3`, `5 : 5`, `0b111 : 7`.

== Construction of Scalars from Prefix Operators

The type-checker currently handles the prefix operators `!` (logical not) and `+`, and `-`.
The `+` sign is a no-op, it's included in the language for cases where it might improve clarity.
`-e` constructs the negative of `e`: it's equivalent to the expression `0 - e`.
`!` has three cases: `!a : 0` if the type of `a` does not contain `0`, `!a` is `1` if the type of `a` only contains `0`, and `!a : 0 | 1` otherwise.

== Construction of Scalars from Arithmetic Operators

Addition and subtraction operators (`+` and `-`) produce a type representing every possible sum of the operands' types, and no more.
For example, given a variable `a: 1..3`, and a variable `b: -5 | -7`, then the expression `a + b` has the type `-6..-2`.
#align(center)[
  #include "examples/scalar-addition.typ"
]

For performance, multiplication and division produce only a contiguous range from the minimum possible result to the maximum.
So, re-using the variables defined above, we find `a * b` has the type `-21..-5`, even if the expression can only produce $-21, -15, -14, -10, -7, " and " -5$.

== Construction of Scalars from Comparison & Logical Operators

Unconditionally, all comparison operators: `<`, `<=`, `>`, `>=`, `==`, and `!=` synthesize to the type `0 | 1`.
Similarly, logical "and" (`&&`), and logical "or" (`||`) always synthesizes to the type `0 | 1`.

Although a scheme similar to the logical not (`!`) operator could be implemented, where the constructed type depends on the operands, a simple implementation was chosen for development efficiency, and to see how it would effect programming in the language.
Ultimately, the difference rarely matters, since if the outcome of a boolean operation is always true, or always false its likely either for debugging, or an error on the programmer's part, making the case relatively rare.


== Future Work<sc-future-work>

The largest missing piece are bit-wise operations.
Due to the internal representation of integer sets (discussed in @sc-disjoint-integer-sets), it is difficult to compute bit-wise "XOR", "AND", and "OR" operations.


= Disjoint Integer Sets<sc-disjoint-integer-sets>

Integer sets are used throughout the type checker. 
As described in @sc-scalars, these sets must be able to every possible result of addition, subtraction, and _at least_ determine the upper and lower bounds of other operations.
Further, the type checker will often test subset relations between sets, and union a series of sets.

There are many solutions for storing large disjoint sets of integers: in particular we investigated Roaring Bitmaps (@chambiBetterBitmapPerformance2016), and Range Set Blaze (@kadieCarlKCarlKRangesetblaze2024).
The set representation was developed with the intention of tracking the exact results of multiplication, and division not just addition and subtraction.
To this end, we chose using a list of stepped ranges, instead of continuous ranges like Range Set Blaze. 
However, this representation made simple operations, like subset difficult, so, to optimize cases where we have sets of arbitrary values, we also give the option of using a large uncompressed bit map.
Finally, to optimize the typical case, where the programmer is performing arithmetic on a large continuous range, sets may be represented just using the two endpoints.

// Internally, we use 3 set representations, _Stripe Sets_, _Small Sets_ and _Continguous Sets_

The following sections give a more detailed overview of the three representations.

== Stripe Sets<sc-stripe-sets>

A _Stripe Set_ is a collection of _step ranges_. 
A step range is some set which includes a minimum element $A$, a maximum element $B$, and every $N^"th"$ element between the two.
For example, we could have a range with a step of $5$, from $0$ to $15$, which includes $0$, $5$, $10$, $15$

Formally, we define $"STEP"(A, B, S) := { n(S) + A : n in NN, n <= (B - A)\/S }$, where $A, B in ZZ, A <= B$ and $S in ZZ, S >= 1, (B - A mod S) equiv 0$.

#let STEP = "STEP"

#let stripe(..rs) = {
  let elems = rs.pos().map(((a, b, s)) => range(a, b, step: s).map(x => str(x))).flatten().join(", ")
  ${ #elems }$
}

In order to add two step ranges:  $alpha", and " beta$ we take the one with the fewest elements (say $alpha$, for this example). 
For every element $a$ in $alpha$, create a new range $STEP("min"(beta) + a, "max"(beta) + a, "step"(beta))$. Multiplication and division falls to only operating on the set's minimum and maximum, in order to construct a new continuous set.

This representation is the most general - it can express any arbitrary set of integers.
But, the in-memory representation can be difficult to manage.
Operations like union and set subtraction can cause the internal representation to become fragmented - several step ranges are used to express a collection of values that could be expressed with a single step range. Care is taken to avoid this fragmentation, or correct it when detected, however the algorithm is far from perfect. During development we found taking union of roughly $60$ continuous ranges had a large decrease in performance.


== Small Sets<sc-sm-set>

A small set is a 1KiB array of bits, with an arbitrary offset.
A value $N$ is included in the set if the bit at index $(N - "offset")$ is set.

This was originally conceived as a way to help with bit-wise operations.
Ideally, the small size, but quick set operations would be a good trade-off for representing enumerable types or bit flags.
However, it is still difficult to compute every possible result of a particular bit-wise operation between two small sets, making them unfit for this use case.
Since enumerable types and bit-flags typically only have a relatively small set of defined values, it would likely be more efficient to use an array of integers.

In the current iteration of the type checker, small sets are often used to store the type of string literals. String literals are short-hand syntax for an array of UTF-8 encoded characters. Usually, the element type for these arrays contains several arbitrary integers, all clustered between 0 and 255 (for example: the string "Hello World" has the type [32 | 72 | 85 | 100 | 101 | 108 | 117 | 120; 11])
== Continuous Ranges<sc-contiguous-ranges>

A continuous range is a set with a minimum element $A$, and a maximum element $B$, which includes every integer between the two.
This is the ideal representation, since addition and subtraction can be easily computed.
To precisely compute the possible results of multiplication between two arbitrary ranges is more complicated, but as mentioned above we only get the smallest possible continuous range for multiplication, making the operation relatively cheap.

== Dynamic Representation

The possible values of any scalar are kept as one of the above types, with a discriminator, this structure is called `DynSet`.
The type checker can construct a new `DynSet` in 2 ways:

1. Using a single value, $a$, (e.g. synthesizing a literal). This creates a contiguous range from $a$ to $a$.
2. Using a type range expression, `a..b`, this creates a contiguous range from `a` to `b`.

From the start of its life as a contiguous range, these dynamic sets can be _upgraded_ to a more suitable representation. For example, after taking the union of two dynamic sets with no overlap, they'll be represented as a stripe set.
In the current implementation, sets will never be _downgraded_, although it is theoretically possible.
This can have odd effects on performance, since a simple set of integers may have an overly complex representation.
For example, if we add `0..5` (a continuous range) to the set `0 | 5 | 10 | 15` (a stripe set), the result would be `0..20`, represented as a stripe set.

= Type Narrowing


In addition to flow control, conditionals also allow the programmer to narrow types.
If $x$ is some unsigned number, a statement like `if x < 5 { ... }` will give $x$ the type $0 .. 5$ within the body of the if statement. If the conditional has an attached `else` clause, then the inverse of the condition is used to narrow types within the body of the else clause.
So, the statement `if x < 5 { ... } else { ... }` is semantically equivalent to `if x < 5 { ... }`, followed by `if x >= 5 { ... }`.

In practice, the "narrowed type" of a variable is a third layer on top of the existing assumed and synthesized types. Each scope may define a single narrowed type for a variable, which overrides the synthesized type in that scope alone. Narrowed types are distinct from synthesized types because they are associated with a scope: they have no effect on the variable's type outside of that scope, and only the first narrowed type in the scope heirarchy matters. For example, consider a snippet which narrows the value of $x$ twice:

#wrapped-figure(
left: code-sample(
```hlt
if x < 10 {
  // x has a narrowed type of 0..10
  if x < 5 {
    // x has a narrowed type of 0..5
  }
  // x has a narrowed type of 0..10
}
```),
right: [
  If we instead assigned $x$ to $5$ within the if-statement inner, this would effect its synthesized type, which would be kept past the end of the if-statmeents body.
  In short: Assumed types are axioms, synthesized types observations based on assignment, and narrowed types are observations based on conditions.])

== Loops

Currently, only while loops are supported.
For simplicity, since while loops can run an indefinite number of times they are treated as a black box. When a while loop is encountered, all variables are assigned their assumed type. Meaning, an previous range analysis is ignored.

#include "examples/while-loop-narrowing.typ"

Within the body of the while loop, types are narrowed using the while loop's condition. In @ex-while-loop-narrowing, line 2 we can safely increment $a$, since the condition ensures $a : 0..4$, but the assumed type of $a$ is $0..10$.



== Producing Constraints<sc-contraints>


Constraints are constructed from the condition of if-statements and while loops, by a _model builder_.
Similar to type checking, the model builder reduces the syntax tree bottom-up.
Each node is converted into a _term_.
A term has three possible shapes:

1. An _Atom_: a variable with an offset: for example "x + 3", "y - 10".
2. A _Cond_: a list of clauses, which must all be true
3. A _LinearSum_: some expression in the form $a x + b y + ... + c$, where $a$, $b$, $c$ are constant, and $x$, $y$ are variables.

If an expression cannot be fit in one of the above forms, then the model builder ignores that expression. For example, if $x$, and $y$ are variables, then $x * y$ cannot be expressed in any of the above forms, so this expression would be ignored. Ignored expressions cascade: if the full expression was $(x * y) + 100$, then the model checker would throw away everything, since it cannot reason about $x * y$. However, every condition separated by the `&&` operator is considered separately.

The builder stops at comparison operations and adds it to the list of possible clauses in the model, producing a _Cond_ term.

The aries library [@bit-monnotPlaansAriesToolbox2024] is used for solving constraints. Because the library only supported 32-bit signed integers, and the Howlite type-checker relies on 128-bit signed integers, we maintain a fork of the library with support for 64-bit, and 128-bit integers. Ideally these changes can be added to upstream repository.

== Solving Constraints

While adding constraints, the model builder maintains a list of variables, or data structure fields that may be narrowed. 
Once the aries solver runs, the type checker repeatedly queres the domain of these variables. After each query, the returned range is added to the variable's new domain, then the solver's model is updated to eliminate that range.

#figure(
```py
# begin with an empty assignment for each variable
var_type = IntegerType.empty()

# search for a solution by repeated backtracking all invalid decisions
# then propogating constraints. 
while solver.backtrace_and_propogate():
  for var in variables:
    var_domain = solver.get_domain(var)
    var_type.include_range(var_domain.lower_bound(), var_domain.upper_bound())
```,
caption: "Repeatedly Query the Solver to find all possible solutions",
supplement: "Figure")


Once we have a set of possible assignments for each variable, the solutions are integrated back into the type checker.
This process is similar to assignment.
If an entire variable is narrowed, e.g. $a < 30$, then that variable in assigned a new narrowed type equal to the solution found by the solver, within the relevant scope.

If only a single field of a variable is narrowed, for example `err.kind == 1`, then we copy the variable's type, replace the field with the new type produced by the solver, then process any consequences of that. For example, if the variable's type is a union, and that assignment is illegal in some of the variants, then those variants are thrown away. A possible use-case for this can be seen in the following example of a function to print compiler errors.

#include "examples/narrowing-unions.typ"

Notice we can access both the `err.kind` and `err.filename` fields without narrowing, since those exist on each union variant.
But, in order to get the line number for parse error the variable has to be narrowed by testing the value of `err.kind`.

= Conculsion

The goal of this project was to create a compiler.
Currently, only the type checker is finished.
The project also lacks documentation and testing.
In it's current state, Howlite should be seen as a proof of concept - a test bed for a few particular language features and nothing more. Although we were unsuccessful in completing the compiler, the process of implementing the type checker has been informative.

We found that there is little benefit to using disjoint sets over continous ranges.
Even if it is possible to implement efficiently, the maintenance cost of keeping a system for handling disjoints is to high to make it a worth-while feature.
Fine-grained control over integer types was a useful and interesting feature in two ways: first, it gives the programmer an effective tool to express intent, particularly when indexing arrays or if overflow is unexpected; second, it unifies enumerable types, unusually sized integers (e.g. 48-bit ints), and standard integers into the same mechanism, simplifying the overall language.

Howlite is also an expirement in low-cost approaches to polymorphism (see @sc-polymorphism). Our two approaches were parametric polymorhpism without specialization and sub-type polymorphism via structural typing.
The parametric polymorphism was a bit difficult to work with: becuase nearly all programming languages generate a different implementation based on the type parameters our entirely different semantics could lead to confusion.
A simpler approach, like the one in Cyclone [@cyclone_types] would be more effective in a production language. Structural typing was much more effective.
It worked well with unions and integer types, as seen in @ex-type-narrowing-and-unions, futher, it allowed functions to only the require the data they used, no matter what the caller might be working with.
The trade-off is again, because we operate at a lower level, the types have to be perfectly aligned to be subtypes.
So, some things that the programmer may not expect to matter intuitively effect subtype relationships, most notably field order.
