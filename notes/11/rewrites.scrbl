#lang scribble/manual

@(require "../fancyverb.rkt" "../utils.rkt")
@(require redex pict scribble-math)
@(require (for-label racket rackunit))
@(require scribble/examples racket/sandbox)

@(define codeblock-include (make-codeblock-include #'here))

@(ev '(require rackunit))
@(for-each (λ (f) (ev `(require (file ,(path->string (build-path "examples" "environments" f))))))
	   '("interp.rkt"))
@(ev '(define (eval e) (interp '() e)))

@(define core-racket
  (parameterize ([sandbox-output 'string]
                 [sandbox-error-output 'string]
                 [sandbox-eval-limits '(0.25 50)])
    (make-evaluator 'racket)))

@(core-racket '(require racket/match))

@(define-syntax-rule (evalsym)
  (scale (text "⇓") 1.5))

@(define-syntax-rule (ex e ...)
  (filebox (emph "Racket REPL")
    (examples #:eval core-racket #:label #f e ...)))

@(define (shellbox . s)
   (parameterize ([current-directory (build-path "examples" "con")])
     (filebox (emph "shell")
              (fancyverbatim "fish" (apply shell s)))))

@title[#:tag "Rewrites"]{Programs as Data Structures}

@table-of-contents[]

@section{Computing over Programs}

Until now, we have only written interpreters, i.e., programs that consume our
programs and produce a value in our language. However, we have no reason to be
limited to just an interpreter. We can take this concept further and build other
kinds of programs that manipulates programs.

In fact, in @secref{Lambda} to evaluate programs, we used:

@itemlist[
@item{@tt{free?} queried expressions to check if a variable is a free variable;}
@item{@tt{alpha-reduce} rewrote expressions to rename certain variables; and}
@item{@tt{beta-reduce} substituted variables with their values in an expression.}
]

Crucially, all these functions operated on a program, but unlike our interpreter
did not return a @emph{value}. We will use similar ideas here and look at
general style of writing such functions.

In general, these take the form of tree walking algorithms (equivalent to doing
a depth-first search on a tree). This will be demonstrated by the following
examples.

@section{Queries on Programs}

Often, there are programming tools that compute some queries over a given
program. As an example, consider a query that computes a list of the integer
constants used through a given program. We will define it as a depth-first style
tree traversal---except, the nodes in our tree will be the subexpressions. To do
this, we need to write a pattern match with cases handling our entire language:

@#reader scribble/comment-reader
(examples #:eval ev
(define (list-ints e)
  (match e
    [(? integer?)            `(,e)]
    [(? boolean?)            '()]
    [(? symbol?)             '()]
    [`(λ (,x) ,e)            (list-ints e)]
    [`(add1 ,e)              (list-ints e)]
    [`(sub1 ,e)              (list-ints e)]
    [`(zero? ,e)             (list-ints e)]
    [`(+ ,e1 ,e2)            (append (list-ints e1) (list-ints e2))]
    [`(- ,e1 ,e2)            (append (list-ints e1) (list-ints e2))]
    [`(* ,e1 ,e2)            (append (list-ints e1) (list-ints e2))]
    [`(/ ,e1 ,e2)            (append (list-ints e1) (list-ints e2))]
    [`(<= ,e1 ,e2)           (append (list-ints e1) (list-ints e2))]
    [`(and ,e1 ,e2)          (append (list-ints e1) (list-ints e2))]
    [`(if ,e1 ,e2 ,e3)       (append (list-ints e1) (list-ints e2) (list-ints e3))]
    [`(let ((,x ,e1)) ,e2)   (append (list-ints e1) (list-ints e2))]
    [`(,e1 ,e2)              (append (list-ints e1) (list-ints e2))]
    [_                       (error "Parser error!")]))
)

Whenever @tt{list-ints} comes across an integer, it returns the integer as a
list. For booleans and variables, it returns an empty list as these are not
integers. All other cases just compose this result from the integer values
correctly. For example, @racket[+], @racket[*], @racket[if], @racket[let],
@racket[λ], etc. all themselves will not contain integer constants, but their
subexpressions may. So these cases recursively call @tt{list-ints} on the
subexpressions and append all the results from subexpressions together.

@#reader scribble/comment-reader
(examples #:eval ev
(list-ints '(let ((x (add1 67)))
              (/ 6 x)))
(list-ints '(let ((foo (λ (x) (λ (y) (+ (+ x 2) y)))))
              (add1 (sub1 (if (zero? 6)
                              (foo 4)
                              5)))))
)

We can craft any kind of query that may be useful to compute over programs. A
common question often asked is how long is my program? We can write a @tt{size}
function that counts the number of terms in the program.

@#reader scribble/comment-reader
(examples #:eval ev
(define (size e)
  (match e
    [(? integer?)            1]
    [(? boolean?)            1]
    [(? symbol?)             1]
    [`(λ (,x) ,e)            (+ 2 (size e))]
    [`(add1 ,e)              (+ 1 (size e))]
    [`(sub1 ,e)              (+ 1 (size e))]
    [`(zero? ,e)             (+ 1 (size e))]
    [`(+ ,e1 ,e2)            (+ 1 (size e1) (size e2))]
    [`(- ,e1 ,e2)            (+ 1 (size e1) (size e2))]
    [`(* ,e1 ,e2)            (+ 1 (size e1) (size e2))]
    [`(/ ,e1 ,e2)            (+ 1 (size e1) (size e2))]
    [`(<= ,e1 ,e2)           (+ 1 (size e1) (size e2))]
    [`(and ,e1 ,e2)          (+ 1 (size e1) (size e2))]
    [`(if ,e1 ,e2 ,e3)       (+ 1 (size e1) (size e2) (size e3))]
    [`(let ((,x ,e1)) ,e2)   (+ 2 (size e1) (size e2))]
    [`(,e1 ,e2)              (+ (size e1) (size e2))]
    [_                       (error "Parser error!")]))
)

We are recursively calculating the size of each subexpression and adding
@racket[1] or @racket[2] based on how many other keywords or identifiers were
there in the term.

We can take any program and ask how long is the program:

@#reader scribble/comment-reader
(examples #:eval ev
(size '(let ((x (add1 67)))
              (/ 6 x)))
(size '(let ((foo (λ (x) (λ (y) (+ (+ x 2) y)))))
              (add1 (sub1 (if (zero? 6)
                              (foo 4)
                              5)))))
(size '(let ((foo 6))
              (add1 (sub1 (if (zero? 6)
                              (foo 4)
                              5)))))
)

Notice the last example carefully, we declared @tt{foo} as @racket[6], but we
called @tt{foo} as a function. Our @tt{size} query ran perfectly fine!
@tt{size} works only with the given program's @emph{syntax}. It did
not have access to the interpreter, thus does not care about the programs
semantics. It did not check if a function is being applied or some other value.

@section{Program Rewrites}

We can also write functions that run over programs and rewrites individual terms
to be something else (just like @tt{alpha-reduce} and @tt{beta-reduce}). It
turns out to be useful in a variety of scenarios:

@subsection{Elaboration}

Until now, the recipe we used to design languages broadly fell in these 3 steps:

@itemlist[
@item{Design a syntax for a language feature}
@item{Give semantics to the language}
@item{Implement semantics in the interpreter}
]

We repeated this over and over again as we added new features to the language.
Many times, however, new language features do not add any new expressive power
to the language. In such cases it should be possible to rewrite a term to
something we can already express in the language. Let us look at it with an
example. We added @racket[let] bindings first, followed by @racket[lambda]
functions. But, lambda functions allow us to represent let bindings easily:

@#reader scribble/comment-reader
(examples #:eval ev
(let ((x (add1 5)))
  (+ x 6))
((λ (x) (+ x 6)) (add1 5))
)

or, more generally,

@racket[(let ((x e1)) e2)]

is equivalent to

@racket[((λ (x) e2) e1)]

We could simplify our interpreter by removing all the places we handle
@racket[let] and by just rewriting all @racket[let]-bindings to be represented
using @racket[lambda]s. This means less code (and bugs) in the interpreter. Our
interpreter then represents only a smaller version of the @emph{surface
language}, called the @emph{core language}! The process of rewriting a surface
syntax to a smaller core language is called @emph{elaboration}. The elaborator
usually is simpler than an interpreter and it usually needs to work only about
the new feature. The original language need not be verified again. Tested
certainly, but because we are not truly adding anything to the language except
syntax all we need to worry about is the new definitions and whether they
implement the new feature correctly. This strategy is used in Haskell, where the
Haskell compiler (GHC) implements only a smaller core language and programs in
the Haskell surface syntax are reduced to the core language.

We can do something similar for the let-binding example above:

@#reader scribble/comment-reader
(examples #:eval ev
(define (elaborate-let e)
  (match e
    [(? integer?)            e]
    [(? boolean?)            e]
    [(? symbol?)             e]
    [`(λ (,x) ,e)            `(λ (,x) ,(elaborate-let e))]
    [`(add1 ,e)              `(add1 ,(elaborate-let e))]
    [`(sub1 ,e)              `(sub1 ,(elaborate-let e))]
    [`(zero? ,e)             `(zero? ,(elaborate-let e))]
    [`(+ ,e1 ,e2)            `(+ ,(elaborate-let e1) ,(elaborate-let e2))]
    [`(- ,e1 ,e2)            `(- ,(elaborate-let e1) ,(elaborate-let e2))]
    [`(* ,e1 ,e2)            `(* ,(elaborate-let e1) ,(elaborate-let e2))]
    [`(/ ,e1 ,e2)            `(/ ,(elaborate-let e1) ,(elaborate-let e2))]
    [`(<= ,e1 ,e2)           `(<= ,(elaborate-let e1) ,(elaborate-let e2))]
    [`(and ,e1 ,e2)          `(and ,(elaborate-let e1) ,(elaborate-let e2))]
    [`(if ,e1 ,e2 ,e3)       `(if ,(elaborate-let e1) ,(elaborate-let e2) ,(elaborate-let e3))]
    [`(let ((,x ,e1)) ,e2)   `((λ (,x) ,e2) ,e1)]
    [`(,e1 ,e2)              `(,(elaborate-let e1) ,(elaborate-let e2))]
    [_                       (error "Parser error!")]))
)

Notice the change from the previous that computed some value over programs. Here
our function works over an expression and produces an @emph{expression}. In
contrast, the interpreter works over the program and produces a @emph{value}.

Here we are recursively calling @tt{elaborate-let} on the subexpressions on all
terms of the program. The real change is only in the @racket[let] case, where it
is rewritten to a function application to @tt{e1} with the function as a lambda
with body as @tt{e2}.

@#reader scribble/comment-reader
(examples #:eval ev
(elaborate-let '(let ((x (add1 5)))
                  (+ x 6)))
)

@subsection{Optimization}

Most, if not all, modern compilers and interpreters perform some kind of
optimization on programs they process. Such optimizations range from simple
function inlining and elimination of constant calculations to sophisticated
variable elimination and loop unrolling. 

This simple example will take a Lambda program and perform optimizations on
numerical calculations. We could write some rules that always hold true for
integers:

@$${
x + 0 = x\\
0 + x = 0\\
x - 0 = x\\
x - x = 0\\
1 * x = x\\
x * 1 = x\\
0 * x = 0\\
x * 0 = 0\\
x / x = 1\\
x / 1 = x\\
}

These rules are not exhaustive, but it gives you an idea of the kind of rules
that can be used for optimization. Let’s define an optimization that replaces
each expression on the left with the corresponding expression on the right.
These are optimizations because the left-hand side has more computations than
the equivalent expression on the right-hand side, which means computing the
right hand side expressions will be faster.

@#reader scribble/comment-reader
(examples #:eval ev
(define (optimize e)
  (match e
    [(? integer?)            e]
    [(? boolean?)            e]
    [(? symbol?)             e]
    [`(λ (,x) ,e)            `(λ (,x) ,(optimize e))]
    [`(add1 ,e)              `(add1 ,(optimize e))]
    [`(sub1 ,e)              `(sub1 ,(optimize e))]
    [`(zero? ,e)             `(zero? ,(optimize e))]
    [`(+ ,e1 ,e2)            (match* ((optimize e1) (optimize e2))
                               [(0 0) 0]
                               [(e3 0) e3]
                               [(0 e4) e4]
                               [(e3 e4) `(+ ,e3 ,e4)])]
    [`(- ,e1 ,e2)            (match* ((optimize e1) (optimize e2))
                               [(0 0) 0]
                               [(e3 0) e3]
                               [(e3 e4) (if (equal? e3 e4)
                                            0
                                            `(- ,e3 ,e4))])]
    [`(* ,e1 ,e2)            (match* ((optimize e1) (optimize e2))
                               [(0 _) 0]
                               [(_ 0) 0]
                               [(1 e4) e4]
                               [(e3 1) e3]
                               [(e3 e4) `(* ,e3 ,e4)])]
    [`(/ ,e1 ,e2)            (match* ((optimize e1) (optimize e2))
                               [(e3 1) e3]
                               [(e3 e4) (if (equal? e3 e4)
                                        1
                                        `(/ ,e3 ,e4))])]
    [`(<= ,e1 ,e2)           `(<= ,(optimize e1) ,(optimize e2))]
    [`(and ,e1 ,e2)          `(and ,(optimize e1) ,(optimize e2))]
    [`(if ,e1 ,e2 ,e3)       `(if ,(optimize e1) ,(optimize e2) ,(optimize e3))]
    [`(let ((,x ,e1)) ,e2)   `(let ((,x ,(optimize e1))) ,(optimize e2))]
    [`(,e1 ,e2)              `(,(optimize e1) ,(optimize e2))]
    [_                       (error "Parser error!")]))
)

@margin-note{All optimizations have to be semantics preserving, i.e., the
behavior of a program should not change after optimizations are applied. While
the examples of optimization we have here are fairly simple, in general it is
non-trivial to design optimizations and prove that they preserve the program
behavior for all programs in the language.}

Again, our @tt{optimize} routine is calling itself recursively on every
subexpression. Notice all the optimizations we wrote are for the arithmetic
operations @racket[+], @racket[-], @racket[*], and @racket[/], so our
optimizations are only limited to these operations. We encode the optimization
rules for all 4 arithmetic operations here. Each subexpression is optimized and
then checked if they fit the pattern expected in the rules. For example, if
@racket[0] is added to any term, it results in the same term. However, if none
of the optimization rules apply, we still have to return the term with the
@racket[+] operation so that the program meaning stays same. Similarly, for
@racket[-] and @racket[/], we check if both operands are same in which case we
return @racket[0] and @racket[1] respectively. The key thing to note is
@tt{optimize} returns an optimized @emph{program} not a value like the
interpreter. As we are rewriting programs, we have to take care to ensure we
produce a program.

We can run our optimizer to check if it behaves as expected:

@#reader scribble/comment-reader
(examples #:eval ev
(optimize '(+ 5 (- 6 0)))
(optimize '(/ 6 (- 6 0)))
(optimize '(λ (x) (* x (* 2 (- x x)))))
)

@section{Testing}

We can convert all the sample programs we ran to check our functions into test
in a similar fashion as we did in earlier classes. However, another way to test
your programs is to think about properties that these rewrites will have and
check if such properties hold on a set of programs.

For example, for the rewrite discussed in @tt{elaborate-let}, one of the key
things is that the program with @racket[let] and without @racket[let] should
agree on the result. Additionally, the elaborated program should not have any
@racket[let] terms remaining after the rewrite. We can capture these conditions
as for all input programs @tt{e}:

@#reader scribble/comment-reader
(examples #:eval ev
(define (check-elaborate-let e)
  (let ((e-new (elaborate-let e)))
    (check-true (and (equal? (eval e) (eval e-new))
                     (no-let? e-new)))))
)

The first clause says that result of evaluation will be equal for both programs,
and the second clause says the new program should not have any @racket[let]
bindings. We can define such a query as follows:

@#reader scribble/comment-reader
(examples #:eval ev
(define (no-let? e)
  (match e
    [(? integer?)            #t]
    [(? boolean?)            #t]
    [(? symbol?)             #t]
    [`(λ (,x) ,e)            (no-let? e)]
    [`(add1 ,e)              (no-let? e)]
    [`(sub1 ,e)              (no-let? e)]
    [`(zero? ,e)             (no-let? e)]
    [`(+ ,e1 ,e2)            (and (no-let? e1) (no-let? e2))]
    [`(- ,e1 ,e2)            (and (no-let? e1) (no-let? e2))]
    [`(* ,e1 ,e2)            (and (no-let? e1) (no-let? e2))]
    [`(/ ,e1 ,e2)            (and (no-let? e1) (no-let? e2))]
    [`(<= ,e1 ,e2)           (and (no-let? e1) (no-let? e2))]
    [`(and ,e1 ,e2)          (and (no-let? e1) (no-let? e2))]
    [`(if ,e1 ,e2 ,e3)       (and (no-let? e1) (no-let? e2) (no-let? e3))]
    [`(let ((,x ,e1)) ,e2)   #f]
    [`(,e1 ,e2)              (and (no-let? e1) (no-let? e2))]
    [_                       (error "Parser error!")]))
)

Then we can test with the properties by writing a few random terms:

@#reader scribble/comment-reader
(examples #:eval ev
(check-elaborate-let '(let ((x (add1 5)))
                        (+ x 6)))
)

If we miss a recursive call somewhere, our checker can spot that without manual
inspection.

For optimizations also, we can come up with a property that both the input and
output programs of the optimizer should agree on the results as the optimizer is
semantics preserving. Additionally, the output of the optimizer should have
same or fewer operations than before. Writing these as a property:

@#reader scribble/comment-reader
(examples #:eval ev
(define (check-optimize e)
  (let ((e-opt (optimize e)))
    (check-true (and (equal? (eval e) (eval e-opt))
                     (<= (size e-opt) (size e))))))
)

We are reusing the @tt{size} function we saw earlier. Now we can throw a bunch
of random programs at this checker to test our optimizer is correct:

@#reader scribble/comment-reader
(examples #:eval ev
(check-optimize '(+ 5 (- 6 0)))
(check-optimize '(/ 6 (- 6 0)))
(check-optimize '(let ((f (λ (x) (* x (* 2 (- x x))))))
                   (f 5)))
)
