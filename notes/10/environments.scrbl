#lang scribble/manual

@(require "../fancyverb.rkt" "../utils.rkt")
@(require redex pict "model.rkt")
@(require (for-label racket rackunit))
@(require scribble/examples racket/sandbox)

@(define codeblock-include (make-codeblock-include #'here))

@(ev '(require rackunit))
@(for-each (λ (f) (ev `(require (file ,(path->string (build-path "examples" "environments" f))))))
	   '("interp.rkt"))

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

@title[#:tag "Env"]{Environments}

@table-of-contents[]

@section{Problems with Substitution}

We have a working language with variables, @racket[let] bindings, and
@racket[lambda]s. Most of your programming experience has taught you that when
a language encounters an identifier, it has to ``look it up''. Not only does our
interpreter not look up anything, we defined its behavior on variables to be an
error! While absolutely correct, this is also a little surprising. More
importantly, we write interpreters to understand and explain languages, and this
implementation might strike you as not doing that, because it doesn’t match our
intuition.

Moreover, if you think about how you use lambdas or functions in a common
language (like JavaScript or Python), it can use variables that were available
to the function at the time of definition, even outside it's scope. The key
contribution of alpha-reduction is to rename such captured variables uniquely
such that you get the same behavior as mainstream languages: ability to use
variables that were available at the time of function definition.

@#reader scribble/comment-reader
(examples #:eval ev
(let ((x 2))
  (let ((fn (λ (n)
              ; x = 2 when fn is applied
              (+ n x))))
    (let ((x 3))
      (fn 5))))
)

In the example above, the function @tt{fn} captures the value of @tt{x} as
@racket[2] and whenever it is applied, the function body gets the value @tt{x}
as @racket[2]. You might know this behavior as @emph{closure}. Substitution gave
us the same behavior as a closure without constructing one, but it does not fit
our mental model of languages.

There’s another problem with substitution, which is the number of times it has
traverse the source program. It would be nice to have to traverse only those
parts of the program that are actually evaluated and only when necessary. But
substitution traverses everything, like unvisited branches of conditionals, and
forces the program to be traversed once for substitution and once again for
interpretation.

In this module, we will migrate to a model of how real languages are implemented
that matches closely with our mental model of variables and closures.

@section{Syntax}

Below is the same syntax we had for our language, but with a few differences:

@centered{@(scale (render-language L) 1.5)}

First, notice @tt{E} in the language? That is the @emph{environment}. It is a
mapping of variables to values stored as a sequence. This fits our mental model:
when we declare variables we @emph{store them} in the environment and when we
evaluate variables we @emph{look up} their values the environment. This also
addresses our second concern about traversing the source program too many times.
Storing a variable in the environment merely denotes our intent to substitute
the identifier later on. Only when we evaluate a variable do we substitute it by
looking up in the environment. In practice, all commonly used languages use the
environment in their implementation.

Second, notice the new value @tt{Closure}? It is a tuple that contains an
environment and an expression. Because lambdas capture the variables declared
before it, we need to store both the environment and the lambda itself in the
closure. Crucially, the lambda expression @racket[(λ (x) e)] is not a value
anymore, it has moved to being an expression that has to be @emph{evaluated} to
yield a closure. Everything else in the language syntax stays the same.

@section{Environments}

We already saw that environments are a mapping of variables to values. Now, to
support storing variable and the values they are bound to and lookups we have
two define two operations on our environment.

First, we define @tt{store} as a function that given an environment, a
variable, and a value to be stored, it adds it to the environment and returns
this @emph{new} environment.

@centered{@(scale (render-store) 1.5)}

Similarly, we will define a function @tt{lookup} that given an environment and a
variable looks up the the environment and returns the value bound to that
variable.

@centered{@(scale (render-lookup) 1.5)}

The @tt{lookup} function, if it finds the variable it is looking for returns the
value bound to it. Otherwise, it recurses and continues the lookup in the
remainder of the environment.

Notice, how the new environment stores the variable binding in the front of the
sequence and how the lookup starts the lookup from the start of that sequence?
This enables the @tt{store} and @tt{lookup} to find the most recently bound
value of a variable in a program.

@#reader scribble/comment-reader
(examples #:eval ev
(let ((x 2))
  ; E = ((x . 2))
  (let ((x 3))
    ; E = ((x . 3) (x . 2))
    x))
)

The program above returns the value @racket[3] which is the most recent value
bound to @tt{x} and our @tt{lookup} and @tt{store} functions work in conjunction
to find the most recent binding.

@section{Semantics with Environments}

With the addition of an environment, the semantics of our language changes
slightly, as shown below. However, this does not change the meaning for most of
the language. The @tt{E ⊢} is now a part of all rules in our language. The
@tt{⊢} symbol is called the
@link["https://en.wikipedia.org/wiki/Turnstile_(symbol)"]{turnstile}. In 
this context, you can read this as: given an environment @tt{E}, @racket[(+ e1 e2)]
means @racket[(+ v1 v2)] if @tt{e1} means @tt{v1} in the same environment @tt{E}
and @tt{e2} means @tt{v2} under the same environment @tt{E}.

@centered{@(scale (render-eval-rules-judgment-1) 1.5)}

The rules are more interesting for rest of the language that actually deal with
looking up or binding new variables.

@centered{@(scale (render-eval-rules-judgment-2) 1.5)}

Unlike substitution, where variables did not have an semantics because they were
substituted away, variables with an environment are looked up in the environment
they are evaluated under. The @tt{var} rules specifies this.

Expressions like @racket[(λ (x) e)] are evaluated to a @tt{Closure} by capturing
the environment it is evaluated with and the lambda expression itself. This is
how closures remember the variables bound before it.

@racket[let] bindings evaluate the binding expression @tt{e1} and store it in
the given environment with the identifier @tt{x} mapping to the resulting value.
This new environment is used to evaluate the body of the @racket[let] binding
@tt{e2} which gives the resulting meaning of the full let binding.

Function applications are defined in the @tt{app} rule. The function position
expression @tt{e1} has to evaluate to a closure for it to be applied to another
expression @tt{e2}. Finally, the body of the lambda stored in the closure is
evaluated with the environment stored in the closure, with additional bindings
of the argument of the lambda (@tt{x}) bound to the argument of the function
application (@tt{v2}).

Note, how these changes do not change the language from the perspective of the
user. The user can write the same programs as they wrote in @secref{Lambda}. All
we are changing is the @emph{implementation strategy}. We can now write an
interpreter that uses environments.

@section{Interpreter with Environments}

We will use Racket's lists to define our environment. One could use Racket's hash tables to implement environments as well.

For our implementation, an empty environment is @racket['()]. Storing a variable
to a value stores a pair in the environment. So an environment containing @tt{x
= 2} will be denoted as @racket['((x . 2))]. Storing @tt{y = 5} in the same
environment, will result in a new environment @racket['((y . 5) (x . 2))]. The
same variables can be redefined as well. If we re-bind @tt{x = 42} in the
environment, it will be stored as @racket['((x . 42) (y . 5) (x . 2))]. We
define a function @tt{store} to define this operation:

@#reader scribble/comment-reader
(examples #:eval ev
(define (store E x v)
  (cons (cons x v) E))
)

Similarly, our look up operation on the environment will walk the list and
return the first occurrence of the identifier. If it is not found it will raise
an error:

@#reader scribble/comment-reader
(examples #:eval ev
(define (lookup E x)
  (match E
    ['() (error "variable not found!")]
    [(cons (cons y v) E) (if (eq? x y) v
                             (lookup E x))]))
)

Notice how the variable not found in our interpreter moved to the @tt{lookup}
function above?

We define @tt{Closure} as a Racket struct that stores the environment and the
expression. This is how the final interpreter looks:

@codeblock-include["environments/interp.rkt"]

All functions related to substitution (@tt{free?}, @tt{alpha-reduce}, and
@tt{beta-reduce}) are gone! The cases in the @tt{interp} function are updated to
call the @tt{store} and @tt{lookup} functions and use @tt{Closure}. Just
like how our inference rules were updated when we defined the meaning of the
language to include an environment, our @tt{interp} and similar functions are
also updated to include the environment @tt{E} as a parameter.

@#reader scribble/comment-reader
(examples #:eval ev
(interp '() '(let ((x 7)) x))
(interp '() '(let ((x 7))
               (let ((x (add1 x)))
                 x)))
(eval:error (interp '() '(let ((x 7))
                           y)))
(interp '() '(let ((x 3))
               (let ((f (λ (n) (+ x n))))
                 (let ((x 5))
                   f))))
)

Running our interpreter now requires us to pass the environment. We just pass an
empty environment @racket['()] to it. The interpreter produce results as
expected. The last example, in particular, demonstrates how a closure captures
the environment (@tt{x = 3} in this case) to be used when the function is called
later.

@section{Testing}

We can convert these to a test cases that can checked automatically:

@#reader scribble/comment-reader
(examples #:eval ev
(check-equal? (interp '() '(let ((x 7)) x)) 7)
(check-equal? (interp '() '(let ((x 7))
                             (let ((x (add1 x)))
                               x)))
              8)
(check-exn exn:fail? (λ ()
                       (interp '() '(let ((x 7))
                                      y))))
(check-equal? (interp '() '(let ((x 3))
                             (let ((f (λ (n) (+ x n))))
                               (let ((x 5))
                                 f))))
              #s(Closure ((x . 3)) (λ (n) (+ x n))))
)


@section{Why Substitution?}

If substitutions and environments result in the same thing, why did we take the
long winded way to learn both?

Substitutions take an expression-centric view on bindings, i.e., all operations
are done at the expression level. Environments, on the other hand, take an
abstract machine view of bindings: there is some environment in any machine
that you can store or do lookup in while interpreting you program.

The former might be tedious to implement but is useful when checking certain
programs for equivalence or doing proofs because all we have to compare is the
expression and nothing else. The latter provides an easier and faster
implementation in practice when building language interpreters.

Having two ways to realize the same thing is great as we can cross-check our
implementation against both to see if they agree. If they do not, at least one
of them is wrong. We can also prove that both approaches will give the same
result for all possible programs in our language, but that is out of scope for
this class. But as an exercise you could compare your two interpreters by
checking if they agree on multiple programs:

@racket[check-equal? (interp-subst e) (interp-env '() e)]

where @tt{e} is some expression in our language. You will have to write some
additional code to correctly compare closures and lambda terms returned by the
substitution based interpreter. You could through a random set of programs at
this checker to build confidence that both are interpreters are indeed
equivalent.
