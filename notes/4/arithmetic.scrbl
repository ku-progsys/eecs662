#lang scribble/manual

@(require "../fancyverb.rkt" "../utils.rkt")
@(require redex pict "model.rkt")
@(require (for-label racket rackunit))
@(require scribble/examples racket/sandbox)

@(define codeblock-include (make-codeblock-include #'here))

@(ev '(require rackunit))
@(for-each (λ (f) (ev `(require (file ,(path->string (build-path "examples" "arithmetic" f))))))
	   '("interp.rkt"))

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

@title[#:tag "Arithmetic"]{Arithmetic: A Language of Numbers}

@table-of-contents[]

@section{Refining the Amount Language}

We have seen all the essential pieces of a language (a grammar, operational semantics, an interpreter, and some correctness tests) for implementing a programming language, although for a very simple one.

We will now, through the process of @bold{iterative refinement} grow the language to be more useful by designing more features.

We will add arithmetic operations to Amount, such that we can now increment, decrement numbers as well as do arithmetic operations such as addition, subtraction, multiplication, and division. We will call this new language Arithmetic. It is still a very simple language, but at least our programs will compute something.

@section{Concrete syntax for Arithmetic}

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

The rule for values is omitted; it's the exact semantics of integers from @secref["Amount"].

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

You can build up a step-by-step proof using the @(evalsym) relation to show that some expression indeed does evaluate to the value produced by the interpreter. For example, evaluation of @racket[(+ 43 (- (add1 23) (sub1 -8)))] will commence as 

@centered{@(scale (renderer (λ () (derivation->pict L (car (build-derivations (eval (+ 43 (- (add1 23) (sub1 -8))) 76)))))) 1.5)}
