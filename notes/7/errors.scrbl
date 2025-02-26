#lang scribble/manual

@(require "../fancyverb.rkt" "../utils.rkt")
@(require redex pict "model.rkt")
@(require (for-label racket rackunit))
@(require scribble/examples racket/sandbox)

@(define codeblock-include (make-codeblock-include #'here))

@(ev '(require rackunit))
@(for-each (λ (f) (ev `(require (file ,(path->string (build-path "examples" "con" f))))))
	   '("interp.rkt"))

@(ev2 '(require rackunit))
@(for-each (λ (f) (ev2 `(require (file ,(path->string (build-path "examples" "defend" f))))))
	   '("interp.rkt"))

@(define (py-repl . s)
  (filebox (emph "Python REPL")
    (apply fancyverbatim "haskell" s)))

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
(eval:error (interp '(/ 5 (sub1 1))))
)

The error shows words like @racket[contract] and @racket[number?]. Our language does not
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

We have been defining the meaning of our languages in terms of a mathematical relation, @(evalsym). Whenever, @(evalsym) relates an expression to a value our language is well defined, i.e., that program has a meaning in our language. Anytime a program cannot be related to a value in @(evalsym) is precisely when our language can crash!

For example, the program @racket[(/ 5 (sub1 1))] crashes with a division by 0 error. We update our language semantics to reflect that division is only defined when the divisor is not 0:

@centered{@(scale (render-eval-rules-judgment) 1.5)}

For arithmetic operations @racket[+], @racket[-], @racket[*], and @racket[/], we should note our evaluation relation @(evalsym) is only defined when the sub-expressions are integers and nothing else.

This is the guiding principle for adding errors to our language: whenever something is not allowed by our formal semantics our language should raise an error! This gives us a meaningful way to sever ties from errors raised by our implementation language (Racket in this case) and give errors that make sense for the user of our language Defend.

@section{Interpreter for Defend}

First, let us see how to raise errors in Racket:

@#reader scribble/comment-reader
(examples #:eval ev2
(eval:error (error "This is an error!"))
)

The @racket[error] function allows us to raise any custom errors. Using this we can now define a wrapper function @tt{only-int}, that checks if the argument is an integer. If it is an integer, it returns the same integer, but raises an error otherwise:

@#reader scribble/comment-reader
(examples #:eval ev
(define (only-int v)
  (if (integer? v)
      v
      (error "Integer expected!")))
)

Here are few examples of it in action:

@#reader scribble/comment-reader
(examples #:eval ev2
(only-int 7)
(eval:error (only-int #t))
)

After that we have to wrap all the cases where the operations are defined on integers with our @tt{only-int} function. Our interpreter for Defend looks like below:

@codeblock-include["defend/interp.rkt"]

Notice, a new function @tt{interp-div}? It throws a division by 0 error if the divisor is 0, or computes the division otherwise. The way we have defined error handling in our language not only protects us against type errors in the language, but also any other errors that may happen. A number divided by 0 is an undefined operation and now we can check and raise such an error gracefully without crashing via the Racket runtime.

Running our interpreter this time:

@#reader scribble/comment-reader
(examples #:eval ev2
(eval:error (interp '(add1 #t)))
(eval:error (interp '(add1 (+ 5 #f))))
(eval:error (interp '(/ 5 (sub1 1))))
)

@section{Correctness}

We can turn the above examples to automatic tests:

@#reader scribble/comment-reader
(examples #:eval ev2
(check-eqv? (interp '(+ 42 (sub1 34))) 75)
(check-eqv? (interp '(zero? (- 5 (sub1 6)))) #t)
(check-eqv? (interp '(if (zero? 0) (add1 5) (sub1 5))) 6)
(check-exn exn:fail? (λ () (interp '(add1 (+ 3 #f)))))
(check-exn exn:fail? (λ () (interp '(add1 (and #t #t)))))
(check-exn exn:fail? (λ () (interp '(/ 5 (sub1 1)))))
)

As we can see our interpreter behaves correctly on all previous examples, but this time also handles all error cases with errors that we defined. The check-exn function checks if an exception was raised by the lambda using the @racket[exn:fail?] predicate. Here we are checking if the last 3 programs result in an error.

@section{Runtime type checking}

Our language was an untyped one until now, but now because of the error handling we just added we have a @emph{runtime type-checked} language, or colloquially known as a @emph{dynamically-typed} language. Instead of giving us undefined behaviors on type errors (or crashes), our language now protects against type errors and fails reliably.

This is the same behavior as industrial grade languages such as Python:

@py-repl{
> 3 + "foo"
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
TypeError: unsupported operand type(s) for +: 'int' and 'str'
}

Runtime type checking catches errors that happen during the evaluation of the program. Thus there is no way to know ahead of time which expression will cause a type error at runtime without running the program. This means, we can still write code ridden with type errors in a runtime type-checked language but still not be aware of it ahead of running our program. Even running our program does not flag all type errors as not all paths are taken during the execution of a program. Consider the example program in our Defend language:

@racket[
(if (zero? (sub1 1)) (+ 2 3) (add1 #f))
]

This program has a type error in the else-branch of the @racket[if], but it will never be caught while running the program. The @racket[if] will always evaluate to @racket[#t] and go to the then-branch.

Static type-checking is an alternative way to address this problem. It can catch type errors in the program without running it, thus it can flag errors in parts of the program that you might not have evaluated when testing it. We will look at static type-checking later in the semester to compare the strengths and weaknesses of the approach in details.
