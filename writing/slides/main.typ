#import "@preview/touying:0.5.3": *
#import themes.simple: *

#show: simple-theme.with(aspect-ratio: "16-9")

= Constrained Integers and Structural Types

== Outline

1. Programming Languages
2. Types
3. Internals

== Programming Languages (Go)

#include "../report/examples/go.typ"



== Programming Languages (Hedy)

#import "../report/templates/example.typ": code-sample, code-example

#let en = ```
print Hello!
ask What is your name?
```

#let ar = align(right, text(dir: rtl, ```
قول Hello!
اسأل What is your name?
```))
#align(center + horizon)[
#grid(columns: 2, rows: 1, gutter: 2em,
    code-example(en, caption: "Hedy (English)"),
    code-example(ar, caption: "Hedy (Arabic)"))
]


== Programming Languages (Howlite)

#let howlite = align(right, ```
func indexOf(s: &[char; NatI32], c: char): 0..NatI32 | -1 {
  let i: Uint32 = 0;
  while i < s.len {
    if str[i] == c {
        return i;
    };
    i = i + 1;
  };
  -1
}
```)

#code-sample(howlite)

== Reducing Cognitive Overhead

1. Generally opt-in features for expressiveness
2. Limited structural Types
3. Constrained integers
4. Well defined overflow

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
- How what happens if an operation overflows? (wrap)


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
- How what happens if an operation overflows? (wrap)



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
- How what happens if an operation overflows? (wrap)


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

== Logical Layer