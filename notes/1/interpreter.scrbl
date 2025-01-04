#lang scribble/manual

@title[#:tag "Intro"]{What @emph{is} an Interpreter?}

Simply put, an interpreter is a function that maps a @emph{program} to a @emph{value}.

@verbatim{
interpreter : Program -> Value
}

In other words, an interpreter gives a program some meaning by executing it with some semantics to compute a value/result.

Some examples of well known interpreters:

@itemlist[
@item{CPython for the Python language}
@item{MRI (or Matz Ruby Interpreter or CRuby) for the Ruby programming language}
@item{V8 (used in Node.js) for JavaScript}
@item{Racket for the Racket programming language}
@item{Intel CPUs for x86 binaries}
@item{Apple M1/M2/M3 for ARM binaries}
]

The last two might be a bit surprising! The first few interpreters are written in software and execute a program to compute a value, where the CPUs are essentially interpreters that execute binaries to compute the final value.

The key requirements for our interpreter are:

@itemlist[
@item{The language of a program should be a valid input to the interpreter}
@item{The interpreter should follow the semantics of the language to execute a given program}
]

@section{What does an interpreter look like?}

An interpreter runs a program through various stages:

@itemlist[#:style 'ordered
@item{@bold{Parsed} into a data structure called the abstract syntax tree (AST)}
@item{@bold{Checked} to see if the source is well formed and sometimes well typed}
@item{@bold{Simplified} to an intermediate representation (IR)}
@item{@bold{Optimized} into an equivalent (but faster) program}
@item{@bold{Evaluated} to compute the result}
]

Steps 3 and 4 are optional depending on the interpreter you are considering/building.

@section{Wait, how is it different from a compiler?}

Compiler is a function that translates a @emph{source program} to a @emph{target program}.

@verbatim{
compiler : SourceProgram -> TargetProgram
}

The key step that is different in a compiler is Step 5: Evaluation. Instead of evaluating a program, a compiler writes the new optimized program in a target language such that it preserves the same behavior (hopefully!). The responsibility of executing the program is left to the interpreter of the target language. You can learn more about compilers in @link["https://compilers.cool/"]{EECS 665: Compiler Construction}.

@section{What is EECS 662?}

EECS 662 aims to teach you about programming languages by building interpreters to demonstrate features and limitations of a programming language while getting acquainted with how language implementations handle these features.

EECS 662 is @bold{not} a class to teach you how to write programs in a variety of programming languages.

Two main goals:

@itemlist[
@item{How to write an interpreter:
    @itemlist[
        @item{Parsing}
        @item{Checking and validation}
        @item{Simplification and normalization}
        @item{Optimization}
        @item{Evaluation}
    ]}
@item{How to write complex programs:
    @itemlist[
        @item{Design}
        @item{Implement}
        @item{Test}
        @item{Iterate}
    ]}
]

@section{Why study programming languages?}

There are a plethora of languages such as Scheme, Prolog, OCaml, or even Smalltalk with very different styles of representing computation as programs. However, most of these languages (and all general-purpose languages) are @emph{turing complete}, i.e., these can compute any function computable by a turing machine. Any program can be written in any language.

Why should we study programming languages then?

@itemlist[
    @item{Studying programming languages will make you a better programmer
    @itemlist[
        @item{Programming is fundamentally a human activity, and language features make it easy or hard to comprehend a specific application}
        @item{Ideas from one language often translate to or are incorporated into other languages}
        @item{Using the right programming paradigm might make your programs easier, faster, and less error-prone}
    ]}
    @item{Become better at learning new languages
    @itemlist[
        @item{Languages allow you to express an idea and shapes how you think when you conceive it}
        @item{You may need to learn a new (or old) language: Paradigms and fads change quickly in CS or you maybe to required to maintain legacy systems}
    ]}
    @item{Language goals have changed over the years
    @itemlist[
        @item{Early days of computing had limited hardware resources. Thus programmers were cheaper than computers. Hence, early language features map easily to hardware concepts (ints, IEEE floats)}
        @item{Now programmers are expensive, but hardware is cheap. Language features encode design concepts (inheritance, encapsulation, assertions), slower languages are okay, security and privacy concerns are more important}
        @item{Future languages will evolve to meet the challenges of coming decades}
    ]}
]

@section{How to write an interpreter?}

We will write our interpreter @emph{gradually, one feature at a time}!

We will:

@itemlist[#:style 'ordered
@item{Start with a tiny language}
@item{Build a full interpreter for it}
@item{Test it}
@item{Add new features}
@item{Go to step 2}
]

@section{Mechanics}

See the @secref{Syllabus}.
