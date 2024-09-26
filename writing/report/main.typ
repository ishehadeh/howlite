#import "@preview/classic-jmlr:0.4.0": jmlr

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

== Prior Art