#import "@preview/touying:0.5.3": *
#import themes.simple: *
#import "../report/templates/example.typ": code-sample, code-example

#show: simple-theme.with(aspect-ratio: "16-9")

= Constrained Integers and Structural Types

== Outline

1. Programming Languages
2. Types
3. Internals


= (1) Programming Languages

== Language Design

- Programming languages are tools for expressing computation
- Key ingrediants in computation: _State_ and _Transitions_
- Programming language is a human-friendly way to express both


== Programming Languages (Ruby)

#let ruby = ```
def indexOf(needle, haystack)
  haystack.chars.each_with_index do |chr, i|
    if chr == needle then
      return i
    end
  end
  
  return -1
end
```

#align(left + horizon)[
  #code-sample(ruby, outline: false)
]

== Programming Languages (Go)

#let go = ```
func indexOf(str string, c rune) int {
	runes := []rune(str);

	for i := 0; i < len(runes); i++ {
		if runes[i] == c { return i; }
	}
	
	return -1;
}
```

#align(left + horizon)[
  #code-sample(go, outline: false)
]


== Programming Languages (Howlite)

#let howlite = align(right, ```
func indexOf(s: &[char; NatI32], c: char): NatI32 | -1 {
  let i: UInt32 = 0;
  while i < s.len {
    if str[i] == c {
        return i;
    };
    i = i + 1;
  };
  -1
}
```)

#code-sample(howlite, outline: false)

= (2) Types

== Types at a Low Level

#let n = 0xDEADBEEF
#let n-bits = range(31, -1, step: -1).map(i => n.bit-and(1.bit-lshift(i)).bit-rshift(i));

#align(center)[
  #math.underbrace([
    #grid(columns: 32, rows: 1, inset: 6pt, stroke: 1pt,
      ..n-bits.map(n => [#n]))
  ], text(size: 40pt, [$"UInt32" =space #n$]))
]

Key Question about Integers:
- How many bits? (32)
- Does it have a sign bit? (no)


== Types at a Low Level

#align(center)[
  #math.underbrace([
    #grid(columns: 32, rows: 1, inset: 6pt, stroke: 1pt,
      ..n-bits.enumerate().map(((i, n)) => [
        #text(fill: if i == 0 { red } else { black })[
          #n
        ]
      ]))
  ], text(size: 40pt, [$"Int32" =space -559038737$]))
]

Key Question about Integers:
- How many bits? (32)
- Does it have a sign bit? (yes)



== Types at a Low Level

#align(center)[
  #grid(columns: 2, rows: 1, align: top,
    [#grid(columns: 16, rows: 1, inset: 6pt, stroke: 1pt, ..n-bits.slice(0, 16).map(n => [#n]))],
    pad(top: 3.5pt)[
    #math.underbrace(grid(columns: 16, rows: 1, inset: 6pt, stroke: 1pt, ..n-bits.slice(16, 32).map(n => [#n]))
    )[#text(size: 40pt, [$"UInt16" =space 48879$])]])
]

Key Question about Integers:
- How many bits? (16)
- Does it have a sign bit? (no)


== Composition

```hlt
type Pair = {
  a: UInt16,
  b: UInt16
}
```

#pause

```
let pair: Pair = #{ a: 0xDEAD, b: 0xBEEF };
```

#pause

#align(center)[
$arrow.b$
#align(center)[
  #grid(columns: 2, rows: 1, align: top,
    [
    #math.underbrace(grid(columns: 16, rows: 1, inset: 6pt, stroke: 1pt, ..n-bits.slice(16, 32).map(n => [#n]))
    )[#text(size: 40pt, [a: UInt16])]],
    [
    #math.underbrace(grid(columns: 16, rows: 1, inset: 6pt, stroke: 1pt, ..n-bits.slice(16, 32).map(n => [#n]))
    )[#text(size: 40pt, [b: UInt16])]])
]
]


== Composition

```hlt
type Triple = {
  a: UInt16,
  b: UInt16,
  c: UInt16
}
```

#pause

```
let triple: Triple = #{ a: 0xDEAD, b: 0xBEEF, c: 0xFEED };
```

#pause

#let m = 0xFEED
#let m-bits = range(15, -1, step: -1).map(i => m.bit-and(1.bit-lshift(i)).bit-rshift(i));

#align(center)[
$arrow.b$
#align(center)[
  #grid(columns: 3, rows: 1, align: top,
    [
    #math.underbrace(grid(columns: 16, rows: 1, inset: 6pt, stroke: 1pt, ..n-bits.slice(16, 32).map(n => [#n]))
    )[#text(size: 32pt, [a: UInt16])]],
    [
    #math.underbrace(grid(columns: 16, rows: 1, inset: 6pt, stroke: 1pt, ..n-bits.slice(16, 32).map(n => [#n]))
    )[#text(size: 32pt, [b: UInt16])]],
    [
    #math.underbrace(grid(columns: 16, rows: 1, inset: 6pt, stroke: 1pt, ..m-bits.map(n => [#n]))
    )[#text(size: 32pt, [c: UInt16])]])
]
]


== Subtyping

#align(center)[
#align(center)[
  #math.overbrace(
    pad(top: 10pt)[
  #grid(columns: (2fr, 1fr), rows: 1,
    [
      #math.overbrace(pad(top: 4pt)[ 
      #grid(columns: 2, rows: 1,
      [
        #math.underbrace(grid(columns: 16, rows: 1, inset: 6pt, stroke: 1pt, ..n-bits.slice(16, 32).map(n => [#n]))
        )[#text(size: 32pt, [a: UInt16])]
      ],
      
      [
        #math.underbrace(grid(columns: 16, rows: 1, inset: 6pt, stroke: 1pt, ..n-bits.slice(16, 32).map(n => [#n]))
        )[#text(size: 32pt, [b: UInt16])]
      ])],
      
      "Pair")
    ],

    pad(top: 23.5pt)[
    #math.underbrace(grid(columns: 16, rows: 1, inset: 6pt, stroke: 1pt, ..m-bits.map(n => [#n]))
    )[#text(size: 32pt, [c: UInt16])]])], 
    "Triple")
]

#v(1.5em)
#grid(rows: 2, gutter: 1em, align: left, 
```hlt
type Triple = { a: UInt16, b: UInt16, c: UInt16 }
```,

```hlt
type Pair   = { a: UInt16, b: UInt16 }
```

)
]

== Adding Another Layer

=== The story so far

- Programs have _State_
- A _Type System_ is a way to describe a program's state

*Integer Types*: _length_ (\# of bits) and _sign_ (can it be negative)

*Structure Types*: A sequence of _named_ fields, each with its own _type_

#pause

=== Static Types for Single States

"This is a 32-bit integer"

"This is an 32-integer, and it has a value of 1"

== Only Using a Few Bits

#let bitgrid = (n, len: 32, cell: (n, _) => [#n], stroke: 1pt) => {
  let bit-values = range(len - 1, -1, step: -1).map(i => n.bit-and(1.bit-lshift(i)).bit-rshift(i));
  grid(columns: len, rows: 1, inset: 6pt, stroke: stroke, ..bit-values.enumerate().map(((i, n)) => cell(n, len - i)))
}

#let highlight(..values) = {
  let values = values.pos()
  (n, i) => if values.contains(i) { text(weight: "extrabold", [#n])} else { [#n] }
}

#align(center)[
  #math.underbrace(bitgrid(1, cell: highlight(1)), text(size: 40pt, [$"1"$]))
]
#v(1em)
#align(center)[
  #math.underbrace(bitgrid(2, cell: highlight(2)), text(size: 40pt, [$"2"$]))
]
#v(1em)
#align(center)[
  #math.underbrace(bitgrid(15, cell: highlight(1,2,3,4)), text(size: 40pt, $space 0..15$))
]

== Intermission

#align(center + horizon)[
  #image("assets/xkcd-heartbleed_explanation-cropped.png", fit: "contain", height: 80%)
]
== Why?
#slide(composer: (1fr, 1.5fr))[
  #only("1-2")[
  1. *Static bounds checks*
  ]
  #only("3-4")[
  1. Static bounds checks
  2. *Expressiveness*
  ]
  #only("5-6")[
  1. Static bounds checks
  2. Expressiveness
  3. *Type Narrowing*
  ]
][
  #alternatives(position: top + left,
  [
    ```
    let buffer: [UInt8; 1024] = ...;
    let start: UInt32 = 256;
    let end: UInt32 = /* user input */;
    &buffer[start..end];
    ^^^^^^^^^^^^^^^^^^^^
    Error! Potential out-of-bounds read.
    ```
  ],

  [
    ```
    let buffer: [UInt8; 1024] = ...;
    let start: UInt32 = 256;
    let end: 256..1023 = /* user input */;
    &buffer[start..end];
    ^^^^^^^^^^^^^^^^^^^^
    Ok now :)
    ```
  ],

  ```go
    // get the N'th bit of a 32-bit int
    func bit(n: UInt32, bit: UInt8): bool

    // get the N'th bit of a 32-bit int
    func bit(n: UInt32, bit: 0..31): 0|1
  ```,
  [
    ```
    type T = { t: 1, payload: UInt32 }
        | { t: 2, payload: Bool }
        | { t: 3, payload: &String }
    ```
  ],
  [
    ```
    type T = { t: 1, payload: UInt32 }
          | { t: 2, payload: Bool }
          | { t: 3, payload: &String }

    ```
    #align(center)[
      $arrow.b.double$
    ]
    ```
    type T = {
      t: 1 | 2 | 3,
      payload: UInt32 | Bool | &String
    }
    ```
  ],
  [
    ```
    type T = { t: 1, payload: UInt32 }
          | { t: 2, payload: Bool }
          | { t: 3, payload: &String }
    ```

    If some instance of `T` has...
    - `t = 1` then `payload` is `UInt32`
    - `t = 2` then `payload` is `Bool`
    - `t = 3` then `payload` is `&String`
  ]
  )
]


= (3) Implementation

== Storing Sets of Integers

*Continuous Ranges*: includes every integer between $L$ and $H$.

#pause

*Small Sets*: includes any set of integers between $O$ and $O + #{8 * 1024}$.

#pause

*Stripe Sets*: A collection of _Step Ranges_

_Step Range_: includes every $N^"th"$ integer between $L$ and $H$

== Small Set

=== Example


#grid(columns: 2, rows: 1, column-gutter: 10pt)[
  $O = 500$
  
  #align(top + right)[$O +$]
][
  #grid(columns: 12, rows: 2, inset: 8pt, align: center, stroke: 1pt,
    ..range(0, 6).map(_ => $0$),
    $1$,
    ..range(0, 2).map(_ => $0$),
    $1$,
    $...$,
    $0$,
    ..range(0, 10).map(n => $#n$),
    $...$,
    $#{8 * 1024 - 1}$
    )
]

This set includes 506, 509.

#pause 

=== Nice Properties

1. Fast set operations (subset, set subtract, union, etc)
2. Reasonably fast element-wise operations.

#import "../report/templates/colors.typ": colors
== Step Range

#let stripe-1 = (l: 7, h: 23, s: 4)
#let stripe-2 = (l: 9, h: 17, s: 2)

#let is-in-stripe = ((l, h, s), n) => n >= l and n <= h and calc.rem(n - l, s) == 0

Set: $L = #{stripe-1.l}, "and" H= #{stripe-1.h}$, with step $#{stripe-1.s}$.

#grid(columns: 24, rows: 2, inset: 5pt, align: horizon + center,
  ..range(1, 25).map(n =>
    if is-in-stripe(stripe-1, n) {
      text(fill: colors.red, $#{n}$)
    } else {
      $#{n}$
    }),
  ..range(1, 25).map(n => {
    if n >= 7 and n <= 23 and calc.rem(n - 7, 4) == 0 {
      $arrow.t$
    }
  })
)

== Stripe Sets



Ranges:
- $L = #{stripe-1.l}, "and" H= #{stripe-1.h}$, with step $#{stripe-1.s}$.
- $L = #{stripe-2.l}, "and" H= #{stripe-2.h}$, with step $#{stripe-2.s}$.


#grid(columns: 24, rows: 2, inset: 5pt, align: horizon + center,
  ..range(1, 25).map(n =>
    if is-in-stripe(stripe-1, n) {
      text(fill: colors.red, $#{n}$)
    } else if is-in-stripe(stripe-2, n) {
      text(fill: colors.blue, $#{n}$)
    } else {
      $#{n}$
    }),
  ..range(1, 25).map(n => {
    if is-in-stripe(stripe-1, n) or is-in-stripe(stripe-2, n) {
      $arrow.t$
    }
  })
)

=== Nice Properties

1. can represent arbitrary sets without too much memory
2. Addition and subtraction are efficient (mostly).

== Problems with Stripe Sets

- Bit-wise operations are slow.
- Stripe sets get fragmented.
- Stripe sets are used too often when other options are better

== Type Checking

#import "@preview/fletcher:0.5.1" as fletcher: diagram, edge
#let fletcher-diagram = touying-reducer.with(reduce: fletcher.diagram, cover: fletcher.hide)

#let node = (..args) => fletcher.node(..args, stroke: args.named().stroke + 2pt, width: 1.5em, height: 1.5em, shape: circle);

#let big = (chr) => align(center + horizon, text(size: 25pt, font: "Noto Sans Mono")[#chr])
#let small = (chr) => align(center + horizon,text(size: 16pt, font: "Noto Sans Mono")[#chr])
 
#grid(columns: (1.15fr, 1fr), gutter: 1em)[
  ```
let x: 0..10 = /* ... */
let y: 0..10 = /* ... */
let z: 0..10 = /* ... */

  ```

  #v(1em)
  #pause
  #grid(columns: 2, gutter: 1em, align: horizon)[
    ```(x + y + z) / 3```
  ][
      #uncover("3-")[
        $stretch(->, size: #160%)^"parses to..."$
      ]
  ]

][
  #pause
#fletcher-diagram(
  node((0, 0), big("/"), stroke: colors.flamingo, name: <div>),
  edge("-|>"),
  node((-.6, 1), big("+"), stroke: colors.lavender, name: <add1>),
  edge("-|>"),
  node((-1.2, 2), big("+"), stroke: colors.overlay0, name: <add2>),
  edge("-|>"),
  node((-1.8, 3), big("x"), stroke: colors.teal),
  edge(<add2>, (-.6, 3), "-|>"),
  node((-.6, 3), big("y"), stroke: colors.teal),

  edge(<add1>, (0, 2), "-|>"),
  node((0, 2), big("z"), stroke: colors.overlay0),


  edge(<div>, (.6, 1), "-|>"),
  node((.6, 1), big("3"), stroke: colors.lavender),

  node-outset: 4pt,
  cell-size: 2em,
  spacing: 1.5em)
]

== Type Checking
#slide(repeat: 7, composer: (1.15fr, 1fr), self => [
   ```
let x: 0..10 = /* ... */
let y: 0..10 = /* ... */
let z: 0..10 = /* ... */

  ```
  #v(1em)
  ```(x + y + z) / 3```

  #par[
  #alternatives[Begin with the bottom leaves][Begin with the bottom leaves][
    Perform operation based on\
    child types
  ][
    Move up a level,\
    get leaf type
  ][
    Perform operation based on\
    child types
  ][
    Move up a level,\
    get leaf type
  ][
    Calculate type of the entire expression
  ]
  ]



], self => [
  #let (uncover, only, alternatives) = utils.methods(self)
#fletcher-diagram(
  node((-1.2, 2), if self.subslide < 3 { big("+") } else { small("0..20")}, stroke: colors.overlay0, name: <add2>),
  edge(<add2>, "-|>", <x>),
  if self.subslide < 1 {
    node((-1.8, 3), big("x"), stroke: colors.teal, name: <x>)
  } else {
    node((-1.8, 3), small("0..10"), stroke: colors.teal, name: <x>)
  },
  edge(<add2>, "-|>", <y>),
  node((-0.6, 3), if self.subslide < 2 { big("y") } else { small("0..10")}, stroke: colors.teal, name: <y>),
  
  node((0, 0), if self.subslide < 7 { big("/") } else { small("0..10")}, stroke: colors.flamingo, name: <div>),
  edge(<div>, "-|>", <add1>),
  node((-.6, 1), if self.subslide < 5 { big("+") } else { small("0..30")}, stroke: colors.lavender, name: <add1>),
  edge(<add1>, "-|>", <add2>),



  edge(<add1>, "-|>", <z>),
  node((0, 2), if self.subslide < 4 { big("z") } else { small("0..10")}, stroke: colors.overlay0, name: <z>),


  edge(<div>, "-|>", <lit3>),
  node((.6, 1), if self.subslide < 6 { big("3") } else { small("3..3")}, stroke: colors.lavender, name: <lit3>),

  node-outset: 4pt,
  cell-size: 2em,
  spacing: 1.5em)
])

== Type Narrowing


`if x + 2 < y { ... }`

== Type Narrowing - Producing Linear Sums

`if x + 2 < y { ... }`

1. Try to build a linear out of each side of the condition

#pause
#h(3em)So we have $x + 2$ on the left and $y$ on the right.

#pause

2. Convert into a simple inequality or equation

#h(3em)The above ineq. is converted into $x - y + 2 < 0$

== Type Narrowing - Determining Implications
#slide(composer: grid.with(columns: (2.5fr, 1fr), gutter: 1em))[
    #uncover("1-")[
      3. Convert the type of $x$ & $y$ into constraints
    ]
    #only("2-")[
      #pad(left: 3em)[Suppose `x: 0..10`\
         and `    y: 2 | 4 | 6 | 8`]
    ]
    #only("4-")[
      4. Repeatedly solve constraints with _aries_
    ]
    #only("6-")[
      5. Reassign variables
      ```
        if x + 2 < y { 
          // y : 4 | 6 | 8 and x : 0..5
      ```
    ]
    #only("7-")[
      6. Process consequences
    ]
][
What we know:
#pad(left: 1em)[
  - $x - y + 2 < 0$
  #uncover("3-")[
  - $0 <= x <= 10$
  - $y <= 8$
  - $y = 2n_1 + 2$
]
  #uncover("6-")[
  - #underline[$y = 2n_2 + 4$]
  - #underline[$x <= 5$]
  ]
]
]

== What can we Learn?

- Range types are likely practical
- Sets of discrete integers are not
- We need a different representation for bit operations

= Questions?