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

@racket[(zero? e)] has two cases. For all expressions @tt{e} @tt{zero-t} rule means @tt{#t} if @tt{e} means {0}. Alternatively, it means @tt{#f} (rule @tt{zero-f}) if @tt{e} is non-zero.

For all expressions @tt{e1} and @tt{e2}, @racket[(and e1 e2)] @(evalsym) @tt{#f}
is in the relation if @tt{e1} @(evalsym) @tt{#f}. Otherwise, the relation has
the same meaning as @tt{e2}. Note, how a definition like this works for both
booleans and integers. @racket[(and #t #t)] is @tt{#t}, @racket[(and #f #t)] is
@tt{#f}, @racket[(and 4 5)] is @tt{5}, and so on.

For all expressions @tt{e1} and @tt{e2}, @racket[(<= e1 e2)] @(evalsym) @tt{#t}
if @tt{e1} means a value less than the meaning of @tt{e2}.

For all expressions @tt{e1}, @tt{e2}, and @tt{e3}, @racket[(if e1 e2 e3)] @(evalsym) @tt{e2} is in the relation if @tt{e1} means some non-false value.
Otherwise @racket[(if e1 e2 e3)] @(evalsym) @tt{e3} is in the relation if @tt{e1} means false.

@section{Interpreter for Con}

We can now translate these operational semantics rules to the interpreter:

@codeblock-include["con/interp.rkt"]

@#reader scribble/comment-reader
(examples #:eval ev
(interp '(+ 42 (sub1 34)))
(interp '(zero? (- 5 (sub1 6))))
(interp '(if (zero? 0) (add1 5) (sub1 5)))
)

@margin-note{You can use builtin Racket functions like @racket[and],
@racket[zero?] instead of writing pattern matches. However, you have to ensure
that the semantics of such builtin functions align with the semantics of the
target language you are implementing. The question then is: are the semantics of
@racket[and] and @racket[zero?] in our language, Con, the same as Racket?}

We can find a one-to-one correspondence between what the interpreter for Con
does and the semantics of the language. Where ever @(evalsym) shows up in the
premise of an operational semantics, it results in recursively calling our
interpreter @racket[(interp ...)]. When we have separate rules that may give
different meaning to the same language construct we use a pattern match and
return the right value.

@section{Correctness}

We can turn the above examples into automatic test cases:

@#reader scribble/comment-reader
(examples #:eval ev
(check-eqv? (interp '(+ 42 (sub1 34))) 75)
(check-eqv? (interp '(zero? (- 5 (sub1 6)))) #t)
(check-eqv? (interp '(if (zero? 0) (add1 5) (sub1 5))) 6)
)

However, unlike Arithmetic it is easy for us to write malformed programs, i.e., programs that do not mean anything.
In other words the meaning of such programs are undefined in our semantics and would most likely crash our interpreter with unexpected error messages or produce unexpected results.

Here are a couple of programs in Con that are valid according to the syntax, but do not mean anything:

@#reader scribble/comment-reader
(examples #:eval ev
(eval:error (interp '(add1 #t)))
(eval:error (interp '(<= #t 7)))
)

Our interpreter right now does not handle errors gracefully and crashes with errors directly from the underlying Racket runtime.
