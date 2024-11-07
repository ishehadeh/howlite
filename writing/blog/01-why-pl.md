# Why Write a Programming Language

This is the first in a series of posts that should - hopefully, help me crystalize ideas about my senior project at St. Mary's College of Maryland.
I'm writing a compiler for a programming language I designed called [Howlite](https://github.com/ishehadeh/howlite).
Before I discuss what's interesting about *my* language, it's important to establish what it means to "make a programming language".

## Differentiating Languages

Programming languages are one of the many, many, interfaces to help us utilize computers.
_Microsoft Word_ helps us compose and format documents, and programming languages help us create algorithms.
Just like word processors, or painting applications, there are countless ways to tweak a programming language's interface to make it more suited to a particular use case or audience.

Here are two quick examples.

### Hedy

Hedy [[2]] is a programming for teaching programming.
It's almost entirely natural language (no scary "{" or "$") and it is translated into many languages (even right-to-left languages!).
Hedy also lets programmers see, or hear the results of their work: it has easily accessible functionality for playing music and drawing graphics.

```hedy
قول Hello!
اسأل What is your name?
```


```hedy
print Hello!
ask What is your name?
```

### Go

Go was an answer to problems with the software infastructure at Google [[1]].
It's designed to be used in large, long-lived software projects.
There's a focus on clear syntax and semantics: no matter who wrote the code, what it does, and how it does it should be clear to any programmer.
It also comes bundled with a tools to help keep programs up to date and consistent.


## Why am I Making a Language

Computers are complicated, I find making them more accessible an interesting challenge.
Programming languages in particular piqued my interest becuase I like programming.
Programming is just solving logic puzzles, anything that let's me look at these problems in a different way is facinating.

The goal of my project in particular was to:

1. Learn about the components of a compiler.
2. Expirement with precise types for scalar values.
3. Applications of sub-type polymorphism and structural typing in resource constrained environments.

There isn't any particularly interesting theory behind any of those goals.
I just threw a bunch of ideas at the wall, and those ware what stuck.
I don't expect Howlite to be a fantastic programming language, but I hope that its unique enough that its particular blend of features might help inform the design of languages I create in the future.


[1]: https://go.dev/talks/2012/splash.article
[2]: https://hedycode.com
