#lang scribble/manual

@(require "../notes/fancyverb.rkt" "../notes/utils.rkt")
@(require (for-label racket rackunit))
@(require scribble/examples racket/sandbox)

@title[#:tag "A4"]{A4: Term Rewrites and Unifying Program State}

@bold{Due:} April 22, 2025

You’ve just joined NebulaSoft, a quirky mid-size company with a custom
programming language that was once the darling of internal tools. Two years ago,
the language's sole creator and self-proclaimed ``rockstar engineer'' left in a
blaze of startup glory, taking with them all knowledge of the language's
internals, documentation, and most of the unit tests.

Since then, the language has been running on life support. People still use
it—begrudgingly—for legacy systems. Over time, users have filed more and more
feature requests, but no one has dared touch the interpreter codebase... until
now.

Users desperately want the expressive power of conditional branching using
@racket[cond]. The language only supports single-argument functions (because the
rockstar thought multi-arg was ``bloated''), so you will have to implement a
rewriter that transforms multi-arg functions into a chain of single-arg
functions. You knowledge from @secref{Rewrites} will be useful here.

Under the hood, the interpreter uses a two-pronged model: a local environment
for variable bindings and a global store for mutable values. Right now, it’s the
programmer’s responsibility to remember when something needs to be allocated in
the store versus when it can live in the environment. It's confusing,
error-prone, and unergonomic. You're going to fix that too.

You'll modify the interpreter so that all bindings—whether local or global—are
handled uniformly via automatic store allocation. The environment will map names
to addresses, and the store will map addresses to values. No more manual
juggling of scopes and storage! Your knowledge from @secref{Env} and
@secref{State} will be relevant here.

@section{Rewrite @racket[cond] to a sequence of @racket[if]}

In @secref{A2}, you added @racket[cond] which enabled you to write a sequence of
conditionals and evaluate them sequentially. To do it, you added a case for
@racket[cond] and added relevant semantics to the interpreter. The goal of this
task is to rewrite @racket[cond] statement to nested @racket[if] expressions so
that it can run in the original interpreter. For example:

@#reader scribble/comment-reader
(examples #:eval ev
(cond [(zero? (- 6 5)) 1]
      [(<= 6 7)        2]
      [else            3])
)

can be written as

@#reader scribble/comment-reader
(examples #:eval ev
(if (zero? (- 6 5))
    1
    (if (<= 6 7)
        2
        3))
)

More generally,

@racketblock[
(cond [e-p1 e-a1]
      [e-p2 e-a2]
      [e-p3 e-a3]
      ...
      [else e-an])
]

can be written as

@racketblock[
(if e-p1
    e-a1
    (if e-p2
        e-a2
        (if e-p3
            e-a3
            ... e-an)))
]

The benefit of rewriting expressive syntax to a simpler term automatically
enables us to keep our interpreter simple, reducing the chances of bugs. To do
this you must:

@itemlist[
@item{Write a function @tt{cond->if} in @tt{rewriter.rkt}. It takes an
expression as an input and produces the rewritten expression with @tt{cond}
replaced by @tt{if}.} 
@item{Add tests in @tt{rewriter.rkt} to check if you handle rewrites correctly
in all kinds of expressions.}
]

@section{Currify Functions}

In functional programming, @emph{currying} is a technique of translating a
function that takes multiple arguments (added in @secref{A3}) into a series of
functions that each take a single argument. This transformation enables more
flexible function composition and partial application. Haskell is a language
that supports partial application. For example, if we have a function @tt{adder}
that adds 2 numbers:

@#reader scribble/comment-reader
(examples #:eval ev
(let ((adder (λ (x y)
               (+ x y))))
  (adder 2 3))
)

A currified version of the same code will look like:

@#reader scribble/comment-reader
(examples #:eval ev
(let ((adder (λ (x) (λ (y)
                      (+ x y)))))
  ((adder 2) 3))
)

A big advantage of a language that allows currying is to enable partial
application:

@#reader scribble/comment-reader
(examples #:eval ev
(let ((adder (λ (x) (λ (y)
                      (+ x y)))))
  (let ((adder2 (adder 2)))
    (adder2 5)))
)

The function @tt{adder2} is built using @tt{adder}, but takes one argument and
always adds @racket[2] to it. One can create such partially applied functions
and can apply it on rest of the arguments later in the program. The goal of this
task is to rewrite lambdas and their applications to the currified version. For
example:

@#reader scribble/comment-reader
(examples #:eval ev
(let ((adder (λ (x y)
               (+ x y))))
  (adder 2 3))
)

will be transformed to:

@#reader scribble/comment-reader
(examples #:eval ev
(let ((adder (λ (x) (λ (y)
                      (+ x y)))))
  ((adder 2) 3))
)

In general,

@racketblock[
(λ (x-1 x-2 ... x-n) e)
]

is converted to:

@racketblock[
(λ (x-1) (λ (x-2) ... (λ (x-n) e)))
]

Similarly, function applications are also transformed as:

@racketblock[
(e-1 e-2 e-3 ... e-n)
]

to 

@racketblock[
(((e-1 e-2) e-3) ... e-n)
]

Doing such a rewrite will allow our language to support partial application
without making any changes to the interpreter.

@itemlist[
@item{Write a function @tt{currify} in @tt{rewriter.rkt}. It takes an expression
as an input and produces the rewritten currified expression.}
@item{Add tests in @tt{rewriter.rkt} to check if you handle rewrites correctly
in all kinds of expressions.}
]

@section{Unifying Program State}

As a programmer in our language, one needs to make a decision of when a local
binding is appropriate vs. when storing something in the state is appropriate.
This is similar to thinking of putting values on the stack or heap. We would
like to simplify this and store everything in the program state. With this
change @tt{new} and @tt{deref} will go away from our language.

So programs like:

@racketblock[
(let ((x (new 5)))
  (seq
   (set x (add1 (deref x)))
   (+ 3 (deref x))))
]

can be written as:

@racketblock[
(let ((x 5))
  (seq
   (set x (add1 x))
   (+ 3 x)))
]

Or the factorial function can be written as:

@racketblock[
(let ((fact #f))
  (seq
   (set fact (λ (n)
               (if (zero? n) 1
                   (* n (fact (- n 1))))))
   (fact 5)))
]

In general:

@itemlist[
@item{Whenever a local variable is bound, it is allocated and stored in the
global state, the binding has the location of the value; and}
@item{Whenever a variable is used, the location is dereferenced and accessed
from the state.}
]

To do this you must:

@itemlist[
@item{Update @tt{interp.rkt} to remove cases for @tt{new} and @tt{deref}, and
update the semantics for @racket[let] to be like @tt{new} and for variable uses
to be like @tt{deref}.}
@item{Add tests to check if your changes behave correctly throughout the
language.}
]

@section{Testing}

You should test your code by writing test cases and adding them to relevant
files. Use the command @tt{raco test [filename]} to test your code.
Alternatively, pressing “Run” in Dr. Racket will also run your test cases.

For grading, your submitted interpreter will be tested on multiple programs
drawn from this language. Writing your own test cases will give you confidence
that your interpreter can handle previously unseen programs.

@section{Submitting}

You should submit on Gradescope. You should submit two files:
@tt{interp.rkt} and @tt{rewriter.rkt} for grading, so make sure all your work is
contained there! You may add any function you need to these files, but do not
rename the file or the @tt{interp}, @tt{cond->if}, and @tt{currify} function.
