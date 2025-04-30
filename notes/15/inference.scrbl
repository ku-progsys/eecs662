#lang scribble/manual

@(require "../fancyverb.rkt" "../utils.rkt")
@(require redex pict)
@(require (for-label racket rackunit))
@(require scribble/examples racket/sandbox)

@(define codeblock-include (make-codeblock-include #'here))

@(ev '(require rackunit))
@(for-each (λ (f) (ev `(require (file ,(path->string (build-path "examples" "types" f))))))
	   '("inference.rkt"))

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

@title[#:tag "Inference"]{Type Inference}

@table-of-contents[]

TODO

@codeblock-include["types/inference.rkt"]

@itemlist[
@item{substitution list = []}
@item{for LHS and RHS in each constraint:
@itemlist[
    @item{continue if LHS == RHS}
    @item{substitute all occurrences of LHS with RHS in both constraint and substitution list and add the substitution LHS -> RHS to substitution list if LHS is not a type}
    @item{substitute all occurrences of RHS with LHS in both constraint and substitution list and add the substitution RHS -> LHS to substitution list if if RHS is not a type}
    @item{create a new constraint mapping domain of LHS with domain of RHS, create a new constraint mapping range of LHS with range of RHS, and add both constraints to constraint list if both LHS and RHS are function types}
    @item{otherwise, it's a type error as LHS and RHS cannot be unified}]}
]
