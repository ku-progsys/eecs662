#lang scribble/manual

@(require "../fancyverb.rkt" "../utils.rkt")
@(require redex pict "model.rkt")
@(require (for-label racket rackunit))
@(require scribble/examples racket/sandbox)

@(define codeblock-include (make-codeblock-include #'here))

@(ev '(require rackunit))
@(for-each (λ (f) (ev `(require (file ,(path->string (build-path "examples" "amount" f))))))
	   '("interp.rkt"))

@(define core-racket
  (parameterize ([sandbox-output 'string]
                 [sandbox-error-output 'string]
                 [sandbox-memory-limit 50])
    (make-evaluator 'racket)))

@(core-racket '(require racket/match))

@(define-syntax-rule (eval)
  (scale (text "⇓") 1.5))

@(define-syntax-rule (ex e ...)
  (filebox (emph "Racket REPL")
    (examples #:eval core-racket #:label #f e ...)))

@(define (shellbox . s)
   (parameterize ([current-directory (build-path "examples" "amount")])
     (filebox (emph "shell")
              (fancyverbatim "fish" (apply shell s)))))

@title[#:tag "Arithmetic"]{Arithmetic: A Language of Numbers}

@table-of-contents[]

@section{Refining the Amount Language}

We have seen all the essential pieces of a language (a grammar, an AST data type definition, operational semantics, an interpreter, and some correctness tests) for implementing a programming language, although for a very simple one.

We will now, through the process of @bold{iterative refinement} grow the language to be more useful by designing more features.

We will add arithmetic operations to Amount, such that we can now increment, decrement numbers as well as do arithmetic operations such as addition, subtraction, multiplication, and division. We will call this new language Arithmetic. It is still a very simple language, but at least our programs will compute something.

@centered{@(scale (render-language L) 1.5)}

@centered{@(scale (render-eval-rules-judgment) 1.5)}
