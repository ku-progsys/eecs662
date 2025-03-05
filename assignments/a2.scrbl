#lang scribble/manual

@(require "../notes/fancyverb.rkt" "../notes/utils.rkt")
@(require (for-label racket rackunit))
@(require scribble/examples racket/sandbox)

@title[#:tag "A2"]{A2: Primitives and Conditionals}

In a forgotten corner of an old electronics lab, an outdated hardware processor
sat, long unused and gathering dust. Once a cutting-edge piece of technology, it
had now become a relic of the past. The processor, though old, was still
functional — its circuits quietly capable, but it had a very limited range of
operations. It could only handle 8-bit signed integers, and it was restricted to
performing basic integer and boolean operations.

Despite these limitations, the processor had one remarkable strength: it
excelled at handling programs with many branches, quickly jumping between
different paths in a program. But to use it effectively, a new programming
language was needed—one that could work within its constraints and take
advantage of its unique ability to handle complex branching efficiently. The
challenge was clear: how could a programmer write code that would unlock the
full potential of this old processor?

The goal of this assignment is to extend the Con language to support these
features. We will call this language @emph{Con+}. This will require you to
change the language in a similar fashion as we have been doing in
class.

You are given a zip file on Canvas @tt{hw2.rkt} with a starter code for the
assignment. This is same as the code for the @secref{Errors} language from
class. You should add your own test cases to ensure your submission is correct.
You are tasked with extending the language to Con+ in the following ways:

@itemlist[
@item{Change the representation of numbers so that denotes only signed 8-bit numbers}
@item{Add few unary and binary primitive operations in the language,}
@item{Add conditional evaluation with @racket[cond]}
]

You have to submit one language implementation with all these changes, and not
multiple languages.

@section{New representation of numbers}

This processor only supports 8-bit signed integers. You have to change Con such that it only operate on numbers in this range:

@itemlist[
@item{Signed 8-bit integers are only integers in the range of -128 to 127 (both inclusive).}
@item{Arithmetic operations such as @racket[+], @racket[-], @racket[*] and @racket[/] operate like standard arithmetic when the results are in this range.}
@item{If any result overflows or underflows this range, it wraps around.}
@item{Any integer values outside this range are not allowed in the language.}
]

For example, a program @racket[345] is not a valid program in our Con+ language. Programs that overflow such as:

@racket[(+ 125 6)]

yields the value @racket[-125]. Similarly, for underflow:

@racket[(- -120 32)]

yields @racket[104].

To do this, you should:

@itemlist[
@item{Extend @tt{hw2.rkt} to update the semantics for these expressions such that values wraparound correctly;}
@item{And, add tests in @tt{hw2.rkt} and run it to see if your programs reflect the above semantics.}
]

@section{New primitive operations}

Add the following forms of expression to the language:

@itemlist[
@item{@racket[(or e e)]: @racket[(or e1 e2)] means @tt{e1} if @tt{e1} does not mean @tt{#f}, else @racket[(or e1 e2)] means the same as @tt{e2}.}
@item{@racket[(- e)]: flips the sign of @tt{e}, i.e., computes @racket[(- 0 e)].}
@item{@racket[(not e)]: compute the logical negation of @tt{e}. Negation of @racket[#f] is @racket[#t], negation of any other value is @racket[#f].}
@item{@racket[(% e e)]: @racket[(% e1 e2)] means the remainder when dividing @tt{e1} by @tt{e2}. The Racket builtin @racket[remainder] will be useful.}
]

To do this, you should:

@itemlist[
@item{Extend @tt{hw2.rkt} to add the semantics for these expressions;}
@item{And, add tests in @tt{hw2.rkt} and run it to see if your programs reflect the above semantics.}
]

@section{Conditional evaluation with @racket[cond]}

The Con language has a simple form of performing conditional evaluation of subexpressions:

@racket[(if e1 e2 e3)]

However, in the original paper on Lisp, @link["http://jmc.stanford.edu/articles/recursive.html"]{Recursive Functions of Symbolic Expressions and Their Computation by Machine, Part I}, John McCarthy introduced a generalization of if called “conditional expressions,” which we will be better suited for this processor. We could add it to our language with the following syntax:

@racket[(cond [e-p1 e-a1] [e-p2 e-a2] ... [else e-an])]

A @racket[cond] expression has any number of clauses @tt{[e-pi e-ai]} …, followed by an @tt{else} clause @tt{[else en]}. For the purposes of this assignment, we will assume every cond expression ends in an else clause, even though this is not true in general. Any cond-expression that does not end in @tt{else} will result in a parser error.

The meaning of a @racket[cond] expression is computed by evaluating each expression @tt{e-pi} in order until the first one that does not evaluate to @racket[#f] is found, in which case, the corresponding expression @tt{e-ai} is evaluated and its value is the value of the @racket[cond] expression. If no such @tt{e-pi} exists, the expression @tt{e-an}’s value is the value of the cond.

Let us understand this with an example:

@#reader scribble/comment-reader
(examples #:eval ev
(cond [(zero? (- 6 5)) 1]
      [(<= 6 7)        2]
      [else            3])
)

The above program first checks if @racket[(zero? (- 6 5))]. As this evaluates to @racket[#f], it goes to the 2nd conditional expression @racket[(<= 6 7)]. As this condition evaluates to @racket[#t], the entire meaning of this @racket[cond] expression is @racket[2] and no further conditional arms (including @tt{else}) are evaluated. The @tt{else} branch is evaluated if no other condition holds.

To implement this, you should:

@itemlist[
@item{Update @tt{hw2.rkt} to correctly interpret cond expressions;}
@item{And, add tests in @tt{hw2.rkt} and run it to see if your programs reflect the semantics of @racket[cond].}
]

@section{Testing}

You should test your code by writing test cases and adding them to relevant files. Use the command @tt{raco test hw2.rkt} to test your code. Alternatively, pressing “Run” in Dr. Racket will also run your test cases. There are few test cases already included in @tt{hw2.rkt}. These are by no means exhaustive, and you should add your own test cases to check your interpreter is correct.

For grading, your submitted interpreter will be tested on multiple programs drawn from this language. Writing your own test cases will give you confidence that your interpreter can handle previously unseen programs.

@section{Submitting}

You should submit on Gradescope. You should submit only one file: hw2.rkt for grading, so make sure all your work is contained there! You may add any function you need to these files, but do not rename the file or the @tt{interp} function.
