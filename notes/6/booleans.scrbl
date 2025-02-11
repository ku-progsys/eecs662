#lang scribble/manual

@(require "../fancyverb.rkt" "../utils.rkt")
@(require redex pict "model.rkt")
@(require (for-label racket rackunit))
@(require scribble/examples racket/sandbox)

@(define codeblock-include (make-codeblock-include #'here))

@(ev '(require rackunit))
@(for-each (λ (f) (ev `(require (file ,(path->string (build-path "examples" "con" f))))))
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
   (parameterize ([current-directory (build-path "examples" "con")])
     (filebox (emph "shell")
              (fancyverbatim "fish" (apply shell s)))))

@title[#:tag "Booleans"]{Con: A Language of Conditions}

@table-of-contents[]

@section{Multiple types of values}

Until now our language produced only one kinds of values: @emph{integers}.
While useful for calculating arithmetic expressions, it is quite limiting.
Let's first address it by introducing boolean values into the language.
We will call our new language @emph{Con}.
The two values we will add are @racket[#t] for true and @racket[#f] for false.
These mirror Racket's boolean values.
Adding boolean values is pointless if our language does not allow us to operate over such values.
So we will add some operations @racket[if], @racket[<=], @racket[zero?], and @racket[and]. We'll describe these operations in detail.

@section{Syntax}

@emph{Con} extends @secref["Arithmetic"] by these operations we described above.
It contains @tt{#lang racket} followed by a single expression.
Our expressions have integers, booleans, and additional operation like @racket[and], @racket[if], @racket[zero?] and so on. The grammar of concrete expression is:

@centered{@(scale (render-language L) 1.5)}

Thus few examples of valid Con programs are:

@codeblock-include["con/example1.rkt"]

@codeblock-include["con/example2.rkt"]

@codeblock-include["con/example3.rkt"]

@section{Meaning of Con Programs}

We will define the meaning of Con programs in natural language. We only define the meaning for the new parts of the language here, the old parts of language stays same.

@itemlist[
@item{The meaning of literal values is just the value itself;}
@item{The meaning of @tt{zero?} is true if the subexpression is @tt{0} or false otherwise;}
@item{The meaning of @tt{and} is false if the first subexpression is false, otherwise it is the result of the second subexpression;}
@item{The meaning of @tt{<=} is a test for if the first subexpression is smaller than or equal to the second subexpression;}
@item{Finally, @tt{(if e1 e2 e3)} means @tt{e3} if @tt{e1} means false, or @tt{e2} otherwise.}
]

We define the operational semantics as before in terms of the @(evalsym) relation.

@centered{@(scale (render-eval-rules-judgment) 1.5)}

Values in our language form the base case in our inductive relation. The @tt{value} rule shows the meaning for integers, @tt{#t}, and @tt{#f} respectively.
