#lang scribble/manual

@(require "../fancyverb.rkt" "../utils.rkt")
@(require redex pict "model.rkt")
@(require (for-label racket rackunit))
@(require scribble/examples racket/sandbox)

@(define codeblock-include (make-codeblock-include #'here))

@(ev '(require rackunit))
@(for-each (λ (f) (ev `(require (file ,(path->string (build-path "examples" "fraud" f))))))
	   '("interp-full.rkt"))

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

@title[#:tag "Let"]{Fraud: Let Bindings and Variables}

@table-of-contents[]

@section{Bindings and Variables}

Now, let us add local bindings to our language. This will allow us to declare a variable, bind a value to it, and use the variable in expressions.

@racket[(let ((id0 e0)) e1)]

This form is called a @racket[let] binding, that binds the @emph{identifier} @tt{id0} to the value of @tt{e0} within the scope of the expression @tt{e1}.

This is a simplification of Racket's let binding form, that allows binding of multiple variables:

@racket[(let ([id0 e0] [id1 e1] ... [idn en]) em)]

that allows you to declare multiple identifiers @tt{id0} through @tt{id1}, binding them to values of expressions @tt{e0} through @tt{e1} respectively in the scope of the expression @tt{em}.

An example program written using let bindings will look like:

@#reader scribble/comment-reader
(examples #:eval ev
(let ((x (add1 6)))
  (let ((y (+ 6 x)))
    (+ x y)))
)

This program binds the value of @racket[(add1 6)] to @tt{x} and sum of @racket[6] and @tt{x} to @tt{y} and then calculates their sum, evaluating to @racket[20].

We will extend our previous language @secref{Errors} to add @racket[let] bindings, and call this new language @emph{Fraud}.

@section{Syntax}

We extend the concrete syntax of Defend with a few forms to support bindings. First, we add @tt{x} to the language, which denotes any @emph{identifier}, i.e., any string that can be represented as a symbol in Racket (alpha-numeric, and symbols like -, +, =, <, >, and similar). These can denote the name of any variable of declarations in our language: Fraud. Second, we add a @racket[(let ((x e)) e)] form to the language. This allows us to declare a new variable @tt{x} and bind the first expression @tt{e} to it, and this @tt{x} is only bound locally in the second expression @tt{e}.

@centered{@(scale (render-language L) 1.5)}

@section{Substitution}

We are done with the syntactic part of extending the language and now move on to defining its semantics:

@itemlist[
@item{the meaning of a let expression @racket[(let ((x e0)) e)] is the meaning of @tt{e} (the body of the @tt{let}) when @tt{x} means the value of @tt{e0} (the right hand side of the @tt{let}),}
@item{the meaning of a variable @tt{x} depends on the context in which it is bound. It means the value of the right-hand side of the nearest enclosing @tt{let} expression that binds @tt{x}. If there is no such enclosing let expression, the variable is meaningless.}
]

Let us understand these rules with a few examples:

@itemlist[
@item{@racket[x]: this expression is meaningless on its own.}
@item{@racket[(let ((x 7)) x)]: this means @tt{7}, since the body expression, @tt{x}, means @tt{7} because the nearest enclosing binding for @tt{x} is to @tt{7}, which means @tt{7}.}
@item{@racket[(let ((x 7)) 2)]: this means @tt{2} since the body expression, @tt{2}, means @tt{2}.}
@item{@racket[(let ((x 7)) (add1 x))]: this means @tt{8} since the body expression, @racket[(add1 x)], means one more than @tt{x} and @tt{x} means @tt{7} because the nearest enclosing binding for @tt{x} is to @tt{7}.}
@item{@racket[(let ((x (add1 7))) x)]: this means @tt{8} since the body expression, @tt{x}, means @tt{8} because the nearest enclosing binding for @tt{x} is to @racket[(add1 7)] which means @tt{8}.}
@item{@racket[(let ((x 7)) (let ((y 2)) x))]: this means @tt{7} since the body expression, @racket[(let ((y 2)) x)], means @tt{2} since the body expression, @tt{x}, means @tt{7} since the nearest enclosing binding for @tt{x} is to @tt{7}.}
@item{@racket[(let ((x 7)) (let ((x 2)) x))]: this means @tt{2} since the body expression, @racket[(let ((x 2)) x)], means @tt{2} since the body expression, @tt{x}, means @tt{2} since the nearest enclosing binding for @tt{x} is to @tt{2}.}
@item{@racket[(let ((x (add1 x))) x)]: this is meaningless, since the right-hand side expression, @racket[(add1 x)] is meaningless because @tt{x} has no enclosing let that binds it.}
@item{@racket[(let ((x 7)) (let ((x (add1 x))) x))]: this means @tt{8} because the body expression @racket[(let ((x (add1 x))) x)] means @tt{8} because the body expression, @tt{x}, is bound to @racket[(add1 x)] is in the nearest enclosing @tt{let} expression that binds @tt{x} and @racket[(add1 x)] means @tt{8} because it is one more than @tt{x} where @tt{x} is bound to @tt{7} in the nearest enclosing @tt{let} that binds it.}
]

Make sure you have a good understanding of how binding works in these examples before moving on. @bold{Remember:} you can always check your understanding by pasting expressions into Racket and seeing what it produces, or better yet, write examples in DrRacket and hover over identifiers to see arrows between variable bindings and their occurrences.

One thing that should be clear from these examples is that the meaning of a sub-expression is not determined by the form of that expression alone. For example, @tt{x} could mean @tt{7}, or it could mean @tt{8}, or it could be meaningless, or it could mean @tt{22}, etc. It depends on the context in which it occurs. So in formulating the meaning of an expression, this context must be taken into account.

A good analogy where context matters for the meaning of an expression is algebra, where it is common to write expressions as given @tt{x = 7 + 3}, find the value of expressions like @tt{2x + 7}. In such cases, you would first find the value of @tt{x} to be @tt{10} and substitute into the given expression to find it is @tt{27}. Substitution is simple for algebra, but for a programming language it is more involved.

Let us define it as a function @tt{subst} that takes the expression @emph{in} which the substitution will take place, @emph{what} variable will be substituted, and @emph{with} the value it will be substituted. Notably, our @tt{subst} function computes a new @emph{expression} from the @emph{in} expression where all occurrences of @emph{what} has been substituted by @emph{with}:

@centered{@(scale (render-subst) 1.5)}

The @tt{subst} function keeps values as-is, as there is nothing to substitute. All the other cases, it does what we expect, recursively applies @tt{subst} to all subexpressions to replace all occurrences. Only two cases are interesting, the ones for the variable @tt{x} and @tt{let} bindings. For the case of variables, we only substitute a variable if it has the same name as the one we are substituting. For @tt{let}, if a let binding is redefining the same variable with a new expression, we only substitute it in the binding expression. We do not substitute in the body of the let, as it should be substituted with the new redefined value, when we give meaning to the redefining let. Conversely, if the let binding is for any other variable, it does not matter and we can substitute in both the binding expression and the body of the let.

@section{Meaning of @tt{let}-bindings}

We can given meaning to @tt{let} expression by the following rule:

@centered{@(scale (render-eval-rules-judgment) 1.5)}

The above rule states, to give meaning to a @tt{let} expression, we first need to give meaning to the binding expressions @tt{e1} as @tt{v1}, then all we need to do is substitute all occurrences of the variable @tt{x} in body of the @tt{let} expression. We will use the @tt{subst} function we defined earlier to denote substitution of all occurrences of @tt{x} with @tt{v1} in @tt{e2} to produce a new term @tt{e3}. The meaning of the entire @tt{let} expression if the meaning of this substituted expression @tt{e3}.

Crucially, we did not give any meaning to variables @tt{x} yet. Look at the case for variables @tt{x} for @tt{subst}, and you will notice all occurrences of a bound variables will be substituted with their values as we give meaning to an expression. That will leave us only with programs, where we have @emph{unbound variables} or @emph{free variables}. For example, @racket[(add1 x)] or @racket[(let ((x 5)) y)] where @tt{x} and @tt{y} are free variables respectively. Such free variables result in an error.

@section{Interpreter for Fraud}

With the formal semantics defined, we can translate that to our interpreter code trivially. First let us translate the @tt{subst} definition to a program. Then we add new cases to the interpreter to reflect the above inference rules using `subst`:

@codeblock-include["fraud/interp.rkt"]

The above interpreter only has two new cases for variables and @tt{let} directly reflecting the rules with @(evalsym) relation. See how we are raising an error because programs with free variables have no meaning?

We can try running this on a few examples:

@#reader scribble/comment-reader
(examples #:eval ev
(interp '(let ((x 7)) x))
(interp '(let ((x 7)) (let ((x (add1 x))) x)))
(eval:error (interp '(let ((x 7)) y)))
)

@section{Testing}

We can write a few cases for the interpreter to see if it behaves as we would expect:

@#reader scribble/comment-reader
(examples #:eval ev
(check-eqv? (interp '(let ((x 1)) (+ x 3))) 4)
(check-eqv? (interp '(let ((x 1))
                        (let ((y 2))
                        (+ x y)))) 3)
(check-eqv? (interp '(let ((x (add1 6)))
                        (let ((x (+ 6 x)))
                        (/ x 2)))) 6)
(check-exn exn:fail? (λ ()
                        (interp '(let ((x (add1 6)))
                                   (let ((x (+ 6 x)))
                                     (/ x y))))))
)
