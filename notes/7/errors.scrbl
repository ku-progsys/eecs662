#lang scribble/manual

@(require "../fancyverb.rkt" "../utils.rkt")
@(require redex pict)
@(require (for-label racket rackunit))
@(require scribble/examples racket/sandbox)

@(define codeblock-include (make-codeblock-include #'here))

@(ev '(require rackunit))
@(for-each (λ (f) (ev `(require (file ,(path->string (build-path "examples" "defend" f))))))
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
   (parameterize ([current-directory (build-path "examples" "defend")])
     (filebox (emph "shell")
              (fancyverbatim "fish" (apply shell s)))))

@title[#:tag "Errors"]{Defend: Handling Errors}

@table-of-contents[]

@section{A crashing language}

We have seen that Con fails for programs that attempt to use boolean values
where numbers should be used or numbers where boolean values should be used.
Before addressing this problem in Con, let's look at how errors should be
handled generally.

We defined a new language called Con using Racket to build the parser and
interpreter. We will refer to Con as the @emph{source language}. We refer to
Racket as the @emph{host language} or implementation language. The distinction
is we are building tools for the language Con, but using Racket to build them.

When the current Con interpreter fails, it fails in our host language - Racket.
While our operational semantics gave precise meaning to the language, whenever
any program deviated from that meaning we did not handle it in the interpreter,
letting the interpreter crash with whatever error Racket produces.

As an example, here is a simple program in our Con interpreter:

@#reader scribble/comment-reader
(examples #:eval ev
(eval:error (interp '(add1 #t)))
)

The error shows words like contracts and @racket[number?]. Our language does not
have contracts or the @racket[number?] predicate! However, things could have
been worse. Here at least our interpreter crashes, but there could be scenarios
where our interpreter computes a wrong value. This approach of failing in our
host language (Racket) works, but we cannot control when our errors happen based
on our language semantics. Moreover, if errors happen, language shows errors
from Racket even though our user was programming in Con. Imagine if you were
writing a Python program and saw and error from C or JavaScript program and saw
a C++ exception? A good language hides implementation details and gives precise
semantics for when errors happen.

Our only choice is to fail completely. What if our interpreters can avoid failures?
What if we can predict failures during execution? This results in systems that
are more robust and code that we can better control. For now, we will design a
language called Defend that will handle errors dynamically or at run-time. Our
interpreter will generate error messages without falling back on our
implementation language.

@section{When should errors happen?}

TODO

@section{Interpreter for Defend}

@codeblock-include["defend/interp.rkt"]
