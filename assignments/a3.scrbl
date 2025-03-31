#lang scribble/manual

@(require "../notes/fancyverb.rkt" "../notes/utils.rkt")
@(require (for-label racket rackunit))
@(require scribble/examples racket/sandbox)

@title[#:tag "A3"]{A3: Let Bindings and Functions}

@bold{Due:} April 7, 2025

In the year 2142, the Intergalactic Data Science Consortium (IDSC) relies on
the @secref{Lambda} language, a general-purpose language used across space
stations and research outposts for analyzing extraterrestrial data. However,
researchers have encountered major inefficiencies in their computations:

@itemlist[
@item{@bold{Lets can binding with one variable:} Every time they process complex
sensor readings, they must use a new @racket[let] for binding each temporary
variable, wasting valuable scientist time writing code.}
@item{@bold{No multi-argument functions:} Every function can only take a single
value, making it cumbersome to model relationships between multiple data points.}
]

The goal of this assignment is to extend the Lambda language to support these
features. We will call this language @emph{Lambda+}. This will require you to
change the language in a similar fashion as we have been doing in
class.

You are given a file on Canvas @tt{hw3.rkt} with a starter code for the
assignment. This is same as the code for the @secref{Lambda} language from
class. You should add your own test cases to ensure your submission is correct.
You are tasked with extending the language to Lambda+ in the following ways:

@itemlist[
@item{Add the general form of @racket[let] bindings}
@item{Add a @racket[let*] form to the language}
@item{Add multi-argument functions and arity checking for function arguments}
]

You have to submit one language implementation with all these changes, and not
multiple languages.

@section{General form of @racket[let] bindings}

Whenever users of the Lambda language need multiple bindings, they have to write
the following:

@#reader scribble/comment-reader
(examples #:eval ev
(let ((x 2))
  (let ((y 3))
    (+ x y)))
)

instead, it would be nicer to collapse all the @racket[let]-s into one as you
would write in Racket:

@#reader scribble/comment-reader
(examples #:eval ev
(let ((x 2)
      (y 3))
  (+ x y))
)

The @racket[let] bindings in our language are a simplification of the general
form of @racket[let]. The general form of let bindings allow you to declare
multiple variables @emph{at once}, like the example above.

The general form of @racket[let] can bind identifiers @tt{id0} through @tt{idn}
to values of expressions @tt{e0} through @tt{en}, locally for the expression @tt{e}:

@racket[
(let ((id0 e0)
      (id1 e1)
      ...
      (idn en))
  e)
]

The meaning of a let expression @racket[(let ((id0 e0) ... (idn en)) e)] is the
meaning of @tt{e} (the body of the @racket[let]) when variables @tt{id0} through
@tt{idn} means the value of @tt{e0} through @tt{en} respectively. The @tt{id}s
are bound ``in parallel.'' That is, no identifier is bound in the right-hand side
expression for any id, but all are available in the body. The ids must be
different from each other. Here is an example to explain this:

@#reader scribble/comment-reader
(examples #:eval ev
(eval:error (let ((x 1)
                  (y (add1 x)))
              (+ x y)))
)

The above results in an error, as declaring @tt{y} requires the @tt{x} to be
defined, but all the newly declared bindings are only available in the body.
Thus @racket[(+ x y)] can be evaluated, but the second binding expression is
invalid in this let form.

This @racket[let] does not allow same identifier to be declared more than once.
So programs like below result in an error:

@#reader scribble/comment-reader
(examples #:eval ev
(eval:error (let ((x 1)
                  (x (add1 x)))
              (add1 x)))
)

Implement this general form of @racket[let].

@section{Add @racket[let*]}

Just like @racket[let] allows variables to be declared ``in parallel'', the
@racket[let*] form allows you declare variables ``in sequence''. Thus, programs
like: 

@#reader scribble/comment-reader
(examples #:eval ev
(let* ((x 1)
       (y 2))
  (+ x y))
)

mean the same if they were written with @racket[let]. However, @racket[let*]
supports latter declarations to be defined in terms of earlier declarations
like:

@#reader scribble/comment-reader
(examples #:eval ev
(let* ((x 1)
       (y (add1 x)))
  (+ x y))
)

The above program is valid, and evaluates to @racket[3].

The general form of @racket[let*] can bind identifiers @tt{id0} through @tt{idn}
to values of expressions @tt{e0} through @tt{en}, locally for the expression
@tt{e}:

@racket[
(let* ((id0 e0)
       (id1 e1)
       ...
       (idn en))
  e)
]

The meaning of a @racket[let*] expression @racket[(let* ((id0 e0) ... (idn en))
e)] is the meaning of @tt{e} (the body of the @tt{let*}) when variables @tt{id0}
through @tt{idn} means the value of @tt{e0} through @tt{en} respectively. The
difference from @tt{let} is that each identifier is available for use in later
expressions, @emph{as well as} in the body. Furthermore, the @tt{id}s need not
be distinct, and the most recent binding is the one that is used.

In the following example:

@#reader scribble/comment-reader
(examples #:eval ev
(let* ((x 1)
       (x (add1 x)))
  (* x 2))
)

first @tt{x} is bound to @racket[1]. This is used to compute @racket[(add1 x)],
i.e., @racket[2] which is bound to the @tt{x} again. Finally, this new value of
@tt{x} is used to compute the body @racket[(* x 2)] which evaluates to
@racket[4].

Implement this form of @racket[let*].

@section{Multi-argument functions and arity checking}

Recall how we have to write multi-argument functions in the Lambda language:

@#reader scribble/comment-reader
(examples #:eval ev
(let ((adder (λ (x) (λ (y)
                      (+ x y)))))
  ((adder 2) 4))
)

defines @tt{adder} that adds two numbers, by defining two lambdas to take two
arguments. Your job for this task is to add native support in the language for
multi-argument functions, so the scientists at IDSC can write programs easily:

@#reader scribble/comment-reader
(examples #:eval ev
(let ((adder (λ (x y)
               (+ x y))))
  (adder 2 4))
)

If a function has @emph{n} formal parameters, it can only be applied to @emph{n}
arguments. The number of arguments to a function is called its arity. If they
mismatch it results in an error. Consider the example: 

@#reader scribble/comment-reader
(examples #:eval ev
(eval:error (let ((adder (λ (x y)
                           (+ x y))))
              (adder 2)))
)

will result in an error, as the function @tt{adder} takes 2 arguments, but only
1 is provided. You should check for function arity when applying the function,
i.e., check if the number of formal arguments and actual arguments are same and
only then evaluate the body of the function. If they are different, raise an
@racket[error] stating arity mismatch.

@section{Testing}

You should test your code by writing test cases and adding them to relevant
files. Use the command @tt{raco test hw3.rkt} to test your code. Alternatively,
pressing “Run” in Dr. Racket will also run your test cases. There are few test
cases already included in @tt{hw3.rkt}. These are by no means exhaustive, and
you should add your own test cases to check your interpreter is correct.

For grading, your submitted interpreter will be tested on multiple programs
drawn from this language. Writing your own test cases will give you confidence
that your interpreter can handle previously unseen programs.

@section{Submitting}

You should submit on Gradescope. You should submit only one file: hw3.rkt for
grading, so make sure all your work is contained there! You may add any function
you need to these files, but do not rename the file or the @tt{interp} function.
