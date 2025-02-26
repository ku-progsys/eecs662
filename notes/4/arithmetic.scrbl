#lang scribble/manual

@(require "../fancyverb.rkt" "../utils.rkt")
@(require redex pict "model.rkt")
@(require (for-label racket rackunit))
@(require scribble/examples racket/sandbox)

@(define codeblock-include (make-codeblock-include #'here))

@(ev '(require rackunit))
@(for-each (λ (f) (ev `(require (file ,(path->string (build-path "examples" "arithmetic" f))))))
	   '("interp.rkt"))

@(ev2 '(require rackunit))
@(for-each (λ (f) (ev2 `(require (file ,(path->string (build-path "examples" "arithmetic" f))))))
	   '("interp-2.rkt"))

@(define core-racket
  (parameterize ([sandbox-output 'string]
                 [sandbox-error-output 'string]
                 [sandbox-memory-limit 50])
    (make-evaluator 'racket)))

@(core-racket '(require racket/match))

@(define-syntax-rule (evalsym)
  (scale (text "⇓") 1.5))

@(define-syntax-rule (ex e ...)
  (filebox (emph "Racket REPL")
    (examples #:eval core-racket #:label #f e ...)))

@(define (shellbox . s)
   (parameterize ([current-directory (build-path "examples" "arithmetic")])
     (filebox (emph "shell")
              (fancyverbatim "fish" (apply shell s)))))

@title[#:tag "Arithmetic"]{Arithmetic: Computing Numbers}

@table-of-contents[]

@section{Refining the Amount Language}

We have seen all the essential pieces of a language (a grammar, operational semantics, an interpreter, and some correctness tests) for implementing a programming language, although for a very simple one.

We will now, through the process of @bold{iterative refinement} grow the language to be more useful by designing more features.

We will add arithmetic operations to Amount, such that we can now increment, decrement numbers as well as do arithmetic operations such as addition, subtraction, multiplication, and division. We will call this new language Arithmetic. It is still a very simple language, but at least our programs will compute something.

@section{Syntax for Arithmetic}

Arithmetic extends Amount to have some unary and binary operations. It contains the @tt{#lang racket} line followed by a single expression. The grammar of concrete expressions is:

@centered{@(scale (render-language L) 1.5)}

So @racket[42], @racket[-8], @racket[120] are all valid programs now. But so are programs like @racket[(add1 42)], @racket[(+ 43 (add1 23))].

An example of a concrete program:

@codeblock-include["arithmetic/example.rkt"]

@section{Meaning of Arithmetic Programs}

The meaning of Arithmetic programs depends of the form of the expression:

@itemlist[
@item{The meaning of the integer literal is the just the integer itself;}
@item{The meaning of the the increment expression (@racket[add1]) is one more than the subexpression;}
@item{The meaning of the the decrement expression (@racket[sub1]) is one less than the subexpression;}
@item{The meaning of the the addition expression (@racket[+]) is the sum of two subexpressions;}
@item{The meaning of the the subtraction expression (@racket[-]) is the difference between the first and second subexpressions;}
@item{The meaning of the the multiplication expression (@racket[*]) is the product of the two subexpressions;}
@item{The meaning of the the division expression (@racket[/]) is the result of dividing the first subexpression by the second.}
]

Note, that Arithmetic is a language of integers, hence all operations should evaluate to integers.

The operational semantics reflects the dependence of the subexpressions as well.

@centered{@(scale (render-eval-rules-judgment) 1.5)}

The rule for values is omitted; it's the exact semantics of integers from @secref{Amount}.

The meaning of the increment (@racket[add1]) and decrement (@racket[sub1])operations depend on only one subexpression. In particular they have @bold{premises} over the line. If the premise is true, the @bold{conclusion} below the line is true as well. These rules are conditional on the premise being true. In contrast, the rule for values applies unconditionally.

Similarly, the addition (@racket[+]) operation depends on two subexpressions. The rules for @racket[-], @racket[*], and @racket[/] all look similar.

Recall that @(evalsym) is a relation. So we can say:

@itemlist[
@item{For all integers @emph{v}, @tt{v} is in @(evalsym);}
@item{For all expressions @emph{e} and integers @emph{v}, if @tt{(e, i)} is in @(evalsym) then @tt{(@racket[(add1 e)], @racket[(+ v 1)])} is in @(evalsym);}
@item{For all expressions @emph{e@subscript{1}}, @emph{e@subscript{2}} and integers @emph{v@subscript{1}} and @emph{v@subscript{2}},  if @tt{(e@subscript{1}, v@subscript{1})} and @tt{(e@subscript{2}, v@subscript{2})} are in @(evalsym) then @tt{(@racket[(+ e1 e2)], @racket[(+ v1 v2)])} is in @(evalsym);}
@item{... and so on!}
]

These rules are inductive. We start from the meaning of integers and if we have the meaning of an expression, we can construct the meaning of a larger expression.

@section{Interpreter for Arithmetic}

We can translate these operational semantics rules to an interpreter:

@codeblock-include["arithmetic/interp.rkt"]

@#reader scribble/comment-reader
(examples #:eval ev
(interp 42)
(interp -8)
(interp '(add1 30))
(interp '(* 2 3))
(interp '(+ 43 (add1 23)))
)

Here's how to connect the dots between the semantics and interpreter: the interpreter is computing, for a given expression @emph{e}, the integer @emph{v}, such that @tt{(e, v)} is in @(evalsym). The interpreter uses pattern matching to determine the form of the expression, which determines which rule of the semantics applies.

@itemlist[
@item{if @emph{e} is an integer @emph{v}, then we're done: this is the right-hand-side of the pair @tt{(e, v)} in @(evalsym).}
@item{if @emph{e} is an expression @racket[(add1 e)], then we recursively use the interpreter to compute @emph{v} such that @tt{(e, v)} is in @(evalsym). But now we can compute the right-hand-side by adding @racket[1] to @emph{i}.}
@item{if @emph{e1} and @emph{e2} are expressions @racket[(+ e1 e2)], then we recursively use the interpreter to compute @emph{v1} and @emph{v2} such that @tt{(e1, v1)} and @tt{(e2, v2)} are in @(evalsym). But now we can compute the right-hand-side by adding @emph{v1} to @emph{v2}.}
@item{... and so on!}
]

This explanation of the correspondence is essentially a proof by induction of the interpreter's correctness.

You can build up a step-by-step proof using the @(evalsym) relation to show that some expression indeed does evaluate to the value produced by the interpreter. All you have to do is use the above formal rules to derive a proof that the relation holds for the expression and the expected value. For example, the proof that @racket[(+ 43 (- (add1 23) (sub1 -8)))] evaluates to @racket[76] will be:

@centered{@(scale (renderer (λ () (derivation->pict L (car (build-derivations (eval (+ 43 (- (add1 23) (sub1 -8))) 76)))))) 1.5)}


Notice how application of each rule creates subgoals? We recursively apply any available rule until we reach the @tt{value} rule. The @tt{value} rule always concludes a subgoal to be proved because it does not depend on any other facts to be true. We discuss this further in @secref{Formal}.

We can now define the correctness of our interpreter:

@bold{Interpreter Correctness:} @emph{For all expressions @racket[e] and integers @racket[v], if @racket[e] @(evalsym) @racket[v], then the interpreter @racket[(interp e)] equals @racket[v].}

@section{Correctness}

We can turn the examples we have above into automatic test cases to verify our interpreter is correct. We will reuse the @racket[check-interp] function from @secref{Amount}.

@#reader scribble/comment-reader
(examples #:eval ev
(define (check-interp e)
  (check-eqv? (interp e) (eval e)))
)

To turn this into an automatic test case:

@#reader scribble/comment-reader
(examples #:eval ev
(check-interp 42)
(check-interp -8)
(check-interp '(add1 30))
(check-interp '(* 2 3))
(check-interp '(+ 43 (add1 23)))
)

The problem, however, is that generating random Arithmetic programs is less obvious compared to generating random Amount programs (i.e. random integers). Randomly generating programs for testing is its own well studied and active research area. To side-step this wrinkle, here is a small utility for generating random Amount programs, which you can use, without needing the understand how it was implemented. Don't worry, you will not be asked to write programs like this in the exam or assignments.

@#reader scribble/comment-reader
(examples #:eval ev
(define (random-expr)
   (contract-random-generate
    (flat-rec-contract b
                       (list/c 'add1 b)
                       (list/c 'sub1 b)
                       (list/c '+ b b)
                       (list/c '- b b)
                       (list/c '* b b)
                       (list/c '/ b b)
                       (integer-in #f #f))))
)

Calling @racket[(random-expr)] now will produce random programs from our grammar:

@#reader scribble/comment-reader
(examples #:eval ev
(random-expr)
(random-expr)
(random-expr)
(random-expr)
)

You can run this in a loop to check if our Arithmetic language interpreter complies with Racket semantics:

@#reader scribble/comment-reader
(examples #:eval ev
(for ([i (in-range 100)])
  (check-interp (random-expr)))
)

We see a bunch of failures because of division by 0, which we did not handle in our language semantics nor our interpreter. We will revisit this again when we look at how to handle errors in @secref{Errors}.

At this point can we find any other counter-example to interpreter correctness that is not division by 0? It’s tempting to declare victory. But... can you think of a valid input (i.e. some program) that might refute the correctness claim?

Think on it.

@section{Does our language always produce integers?}

Values in our language are integers only. However, does our language always produce integers? Let us try this with a very simple program:

@#reader scribble/comment-reader
(examples #:eval ev
(interp '(/ 5 3))
)

Why is it returning values as a fraction? Fractions are not a part of our language! Fractions, or more generally, rational numbers are defined in Racket.

@#reader scribble/comment-reader
(examples #:eval ev
(/ 5 3)
)

Our language Arithmetic looks like Racket, but has different semantics, primarily because our language only has integers. What we are seeing is the bubbling up semantics of Racket, our @emph{source language}, into Arithmetic, our @emph{target language}. This is a common problem in language design: the semantics of your source language will tend to show up in your target language and as a language designer you have to be careful about such cases! For example, Python is implemented in C, but Python supports arbitrary sized integers, whereas C is only limited machine-sized integers. Python developers have to be careful to ensure that C numerics behavior does not show up in Python.

Thankfully, our language Arithmetic is very simple. To fix this, we only have to update the division of two integers to return their @racket[quotient].

@codeblock-include["arithmetic/interp-2.rkt"]

This fixes the semantics of our language:

@#reader scribble/comment-reader
(examples #:eval ev2
(interp '(/ 4 2))
(interp '(/ 5 3))
)
