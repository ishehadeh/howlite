#import "@preview/classic-jmlr:0.4.0": jmlr
#import "@preview/wrap-it:0.1.0": wrap-content

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

#show raw: set text(size: 0.8em)

= Syntax

#let fig = figure(
    rect(
      pad(x: 8pt, y: 8pt)[
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
  ), caption: [If Statements Have Values])

#wrap-content(fig, align: right)[
  Howlite's syntax prioritizes familiarity, ease of parsing, and clarity. The syntax should be familiar, someone unfamiliar with the language should be able to immediately grasp the programmer's intent, event if they do not understand every line. In a similar vein, the programmer should be guided towards writing code that is easily legible by others. We approach this issue by providing language constructs that clearly express intent. For example, flow control constructs, like if statements may have a value. This allows the programmer to clearly show a variable's value is the result of some condition. In order to make tooling easier to write, we prioritize createing an unambigous grammar, with no constructs that require unbounded look-ahead.
]

== Familiarity

Howlite code should be recognizable to C programmers. For this reason, we use curly braces ("{" and "}") to denote blocks of code. We use familiar imperative /* TODO: define imperative */ keywords: "if", "else", and "while", and mathmatical expressions follow typical infix notation. Howlite differs from C in that it requires a sigil character or keyword before beginning a new construct. Types do not lead in variable assignments or functions. Instead we use the "let" or "func" keywords, respectively. This simplifies parsing, since we know what type of statement or expression will follow, similarly, type ascriptions are always prefixed with `:`. These keywords and symbols were decided by surveying popular languages during design. For example, "let", and `:` come from TypeScript, while "func" is a keyword in Go.

== Clarity



