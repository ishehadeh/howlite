#import "@preview/touying:0.5.3": *
#import themes.simple: *
#import "../report/templates/example.typ": code-sample, code-example

#show: simple-theme.with(aspect-ratio: "16-9")

= Constrained Integers and Structural Types

== Outline

1. Programming Languages
2. Types
3. Internals


== Language Design

- Programming languages are tools for expressing computation
- Key ingrediants in computation: _State_ and _Transitions_



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

// == Reducing Cognitive Overhead

// 1. Generally opt-in features for expressiveness
// 2. Limited structural Types
// 3. Constrained integers
// 4. Well defined overflow

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

*Compound Types*: A sequence of _named_ fields, each with its own _type_

#pause

=== Static Types for Single States

"This is a 32-bit integer"

"This is an 32-integer, and it has a value of 1"

== Only Using a Few Bits

#let bitgrid = (n, len: 32, cell: n => [#n]) => {
  let bit-values = range(len - 1, -1, step: -1).map(i => n.bit-and(1.bit-lshift(i)).bit-rshift(i));
  grid(columns: len, rows: 1, inset: 6pt, stroke: 1pt, ..bit-values.enumerate().map(((i, n)) => cell(n, len - i)))
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

#grid(columns: (1fr, 1.5fr), gutter: 0.5em,
[
=== Why do this?

1. *Static bounds checks*
],
[
#image("assets/xkcd-heartbleed_explanation-cropped.png", fit: "contain", height: 80%)
])

== Intermission

#grid(columns: (1fr, 1.5fr), gutter: 0.5em,
[
=== Why do this?

1. *Static bounds checks*
],
[
```
let buffer: [UInt8; 1024] = ...;
let start: UInt32 = 256;
let end: UInt32 = /* user input */;
&buffer[start..end];
^^^^^^^^^^^^^^^^^^^^
Error! Potential out-of-bounds read.
```
])

== Intermission

#grid(columns: (1fr, 1.5fr), gutter: 0.5em,
[
=== Why do this?

1. Static bounds checks
2. *Expressiveness*
],
[
```go
// get the N'th bit of a 32-bit int
func bit(n: UInt32, bit: UInt8): bool

// get the N'th bit of a 32-bit int
func bit(n: UInt32, bit: 0..31): 0|1
```
])

== Intermission

#grid(columns: (1fr, 1.5fr), gutter: 0.5em,
[
=== Why do this?

1. Static bounds checks
2. Expressiveness
3. *Type Narrowing*
],
[
```
type T = { t: 1, payload: UInt32 }
       | { t: 2, payload: Bool }
       | { t: 3, payload: &String }
```
])

== Intermission

#grid(columns: (1fr, 1.5fr), gutter: 0.5em,
[
=== Why do this?

1. Static bounds checks
2. Expressiveness
3. *Type Narrowing*
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
])

== Intermission

#grid(columns: (1fr, 1.5fr), gutter: 0.5em,
[
=== Why do this?

1. Static bounds checks
2. Expressiveness
3. *Type Narrowing*
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
])

