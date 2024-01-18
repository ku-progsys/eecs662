---
layout: page
parent: Notes
title: What is an interpreter?
---

# What is an interpreter?

Simply put, an interpreter is a function that maps a _program_ to a _value_.

```
interpreter : Program -> Value
```

In other words, an interpreter gives a program some meaning by executing it with some semantics to compute a value/result.

Some examples of well known interpreters:

* CPython for the Python language
* MRI (or Matz Ruby Interpreter or CRuby) for the Ruby programming language
* V8 (used in Node.js) for JavaScript
* Racket for the Racket programming language
* Intel CPUs for x86 binaries
* Apple M1/M2/M3 for ARM binaries

The last two might be a bit surprising! The first few interpreters are written in software and execute a program to compute a value, where the CPUs are essentially interpreters that execute binaries to compute the final value.

The key requirements for our interpreter are:

* The language of a program should be a valid input to the interpreter
* The interpreter should follow the semantics of the language to execute a given program

## What does an interpreter look like?

An interpreter runs a program through various stages:

1. **Parsed** into a data structure called the abstract syntax tree (AST)
2. **Checked** to see if the source is well formed and sometimes well typed
3. **Simplified** to an intermediate representation (IR)
4. **Optimized** into an equivalent (but faster) program
5. **Evaluated** to compute the result

Steps 3 and 4 are optional depending on the interpreter you are considering/building.

## Wait, how is it different from a compiler?

Compiler is a function that translates a _source program_ to a _target program_.

```
compiler : SourceProgram -> TargetProgram
```

The key step that is different in a compiler is Step 5: Evaluation. Instead of evaluating a program, a compiler writes the new optimized program in a target language such that it preserves the same behaviour (hopefully!). The responsibility of executing the program is left to the interpreter of the target language. You can learn more about compilers in [EECS 665: Compiler Construction](https://compilers.cool/).

## What is EECS 662?

EECS 662 aims to teach you about programming languages by building interpreters to demonstrate features and limitations of a programming language while getting acquainted with how language implementations handle these features.

EECS 662 is **not** a class to teach you how to write programs in a variety of programming languages.

Two main goals:

* How to write an interpreter:
  * Parsing
  * Checking and validation
  * Simplication and normalization
  * Optimization
  * Evaluation
* How to write complex programs:
  * Design
  * Implement
  * Test
  * Iterate

## Why study programming languages?

There are a plethora of languages such as Scheme, Prolog, OCaml, or even Smalltalk with very different styles of representing computation as programs. However, most of these languages (and all general-purpose languages) are _turing complete_, i.e., these can compute any function computable by a turing machine. Any program can be written in any language.

Why should we study programming languages then?

* Studying programming languages will make you a better programmer
  * Programming is fundamentally a human activity, and language features make it easy or hard to comprehend a specfic application
  * Ideas from one language often translate to or are incorporated into other languages
  * Using the right programming paradigm might make your programs easier, faster, and less error-prone
* Become better at learning new languages
  * Languages allow you to express an idea and shapes how you think when you conceive it
  * You may need to learn a new (or old) language: Paradigms and fads change quickly in CS or you maybe to required to maintain legacy systems
* Language goals have changed over the years
  * Early days of computing had limited hardware resources. Thus programmers were cheaper than computers. Hence, early language features map easily to hardware concepts (ints, IEEE floats)
  * Now programmers are expensive, but hardware is cheap. Language features encode design concepts (inheritance, encapsulation, assertions), slower languages are okay, security and privacy concerns are more important
  * Future languages will evolve to meet the challenges of coming decades

## How to write an interpreter?

We will write our interpreter _gradually, one feature at a time_!

We will:

1. Start with a tiny language
2. Build a full interpreter for it
3. Test it
4. Add new features
5. Go to step 2

## Mechanics

See the [Syllabus]({{site.baseurl}}/syllabus).
