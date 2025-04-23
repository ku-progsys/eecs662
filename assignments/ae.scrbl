#lang scribble/manual

@(require "../notes/fancyverb.rkt" "../notes/utils.rkt")
@(require (for-label racket rackunit))
@(require scribble/examples racket/sandbox)

@title[#:tag "AE"]{AE: Partial Evaluation}

@bold{Due:} May 9, 2025

In this assignment, you will implement a partial evaluator---a program
transformation tool that specializes a program based on known inputs. Partial
evaluation aims to optimize a program by precomputing parts of its execution
when some arguments are known in advance, effectively producing a new, more
efficient version of the original program. The resulting specialized program
runs faster by avoiding redundant computation and simplifying control flow. This
assignment will guide you through the process of designing and implementing a
partial evaluator for a small functional language, helping you understand key
concepts such as environment management, symbolic execution, and residual code
generation.

@itemlist[
@item{The starter code is provided in @tt{pinterp.rkt} file. It is the same
interpreter as the one from the @secref{Env}.}
@item{You have to implement a function @tt{pinterp} that does partial
interpretation given an environment and an expression.}
@item{This will look very similar to the @tt{interp} function, except it
evaluates the expression to a @emph{value} when possible, otherwise just returns
a @emph{residual expression}, i.e., the expression where all the parts that
could have been evaluated have been evaluated.}
@item{The key semantic change is to check if the subexpressions are a value, you
evaluate the current expression, or leave the current expression semantically
unchanged.}
]

Here are a few examples:

@itemlist[
@item{@racket[(pinterp '() '(+ 3 4))] will give @racket[7]}
@item{@racket[(pinterp '() '(+ x (* 2 4)))] will give @racket[(+ x 8)]}
@item{@racket[(pinterp '((x . 5)) '(+ x (* 2 4)))] will give @racket[13]}
@item{@racket[(pinterp '() '(let ((x (sub1 4))) (+ x y)))] will give @racket[(+ 3
y)]}
@item{@racket[(pinterp '() '((λ (x) (λ (y)
                          (if (zero? 0)
                              (+ x y)
                              (- x y)))) 2))] will give @racket[(λ (y) (+ 2 y))]}
]


@section{Testing}

You should test your code by writing test cases and adding them to relevant
files. The starter code is available in @tt{pinterp.rkt} on Canvas. Use the command
@tt{raco test [filename]} to test your code. Alternatively, pressing “Run” in
Dr. Racket will also run your test cases.

For grading, your submitted interpreter will be tested on multiple programs
drawn from this language. Writing your own test cases will give you confidence
that your interpreter can handle previously unseen programs.

@section{Submitting}

You should submit on Gradescope. You should submit one file:
@tt{pinterp.rkt} for grading, so make sure all your work is
contained there! You may add/remove any function you need to these files, as
long as the @tt{pinterp} function is present.
