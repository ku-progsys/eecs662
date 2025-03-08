#lang scribble/manual

@(require "../fancyverb.rkt" "../utils.rkt")
@(require redex pict "model.rkt")
@(require (for-label racket rackunit))
@(require scribble/examples racket/sandbox)

@(define codeblock-include (make-codeblock-include #'here))

@(ev '(require rackunit))
@(for-each (λ (f) (ev `(require (file ,(path->string (build-path "examples" "lambda" f))))))
	   '("interp.rkt"))

@(ev-time '(require rackunit))
@(for-each (λ (f) (ev-time `(require (file ,(path->string (build-path "examples" "lambda" f))))))
	   '("interp.rkt"))

@(ev2 '(require rackunit))
@(for-each (λ (f) (ev2 `(require (file ,(path->string (build-path "examples" "lambda" f))))))
	   '("interp-full.rkt"))

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

@title[#:tag "Lambda"]{Lambda: First-class Functions}

@table-of-contents[]

@section{First-class Functions}

Let us start turning the previous language, Fraud, into a real programming language. Let us say you write an expression to compute a particular value. Such an expression will only work for the single computation you have written down. For example, if you want to compute a factorial of 5, you will have to write a program @racket[(* 5 (* 4 (* 3 (* 2 1))))] that will compute the result as @racket[120]. If you want to find the factorial of any number, you will have to write down infinite number of expressions in Fraud. This means the expressiveness of our language is still severely restricted.

The solution is to bring in the computational analog of inductive data. Just like we can describe our programs by inductively composing each form in our language grammar, our language needs to be able to define arbitrarily long running computations. Crucially, these arbitrarily long running computations need to be described by finite sized programs. The analog of inductive data are recursive functions.

However, we will go one step further that just defining and using functions. We will also return functions and pass functions as arguments to programs, i.e., treat functions as values. Programming with functions as values is a powerful idiom that is at the heart of both functional programming and object-oriented programming, which both center around the idea that computation itself can be packaged up in a suspended form as a value and later run.

These functions in their most general form are called λ-expressions. We will add this to our language and call it @emph{Lambda}.

@section{Syntax}

First, we will look at how λs work in language in isolation and then add it to our main language. Here is the syntax for λ-expression:

@racket[(λ (x) e)]

Here @tt{x} is the @emph{formal parameter} of the function and @tt{e} is the body. Certain things are notable about this function expression:

@itemlist[
@item{There is no function name in the λ-expression; it is an anonymous function.}
@item{This new form is an expression — it can appear any where as a subexpression in a program.}
]

Since functions are also considered values, functions can also be returned from a function. Thus, we can generalize what’s allowed in the function position of the syntax for function applications to be an arbitrary expression. That expression is expected to produce a function value (and this expectation gives rise to a new kind of run-time error when violated: applying a non-function to arguments), which can called with the value of the arguments. Hence the syntax of function applications is:

@racket[(e e)]

In particular, the function expression can be a λ-expression, e.g.:

@ex[
((λ (x) (+ x x)) 10)
]

But also it may be expression which produces a function, but isn’t itself a λ-expression:

@ex[
(let ((adder (λ (n) (λ (x) (+ x n)))))
  ((adder 5) 10))
]

Here, @racket[(adder 5)] is the function position of @racket[((adder 5) 10)]. That subexpression is itself a function call expression, calling @racket[adder] with the argument @racket[5]. It's result is a function that, when applied, adds @racket[5] to its argument. The argument on which the function is applied is called an @emph{actual parameter} to the function. In this case @racket[5] and @racket[10] are the actual parameters.

While these are the key ideas, we will look at lambda expressions in a tiny language:

@centered{@(scale (render-language L) 1.5)}

This above language is the @emph{Lambda Calculus}. The lambda calculus was introduced in the 1930s by the logician Alonzo Church at Princeton University as a mathematical system for defining computable functions. Alan Turing (who was Church’s Ph.D. student) showed that the lambda calculus is equivalent in definitional power to Turing machines. The lambda calculus serves as the model of computation for functional programming languages and has applications to artificial intelligence, proof systems, and logic.

The programming language Lisp was developed by John McCarthy at MIT in 1958 around the lambda calculus. Haskell, considered by many as one of the purest functional programming languages, was developed by Simon Peyton Jones, Paul Houdak, Phil Wadler and others in the late 1980s and early 90s. Many established languages such as C++ and Java and many more recent languages such as Python, Ruby, and JavaScript have adopted lambda expressions as anonymous functions from the lambda calculus.

Because of its simplicity, lambda calculus is a very useful tool for the study and analysis of programming languages. It only has variables @tt{x}, function abstractions @racket[(λ (x) e)], written as @tt{(λx.e)} as a short-hand, and function applications @tt{(e e)}. This is a tiny subset of the language we will design, but this is all we need to define all computable functions!

A @emph{function abstraction} is a lambda expression that defines a function. It consists of four parts: a lambda followed by a variable, a period, and then an expression as in @tt{λx.e}. Here, the variable @tt{x} is the formal parameter of the function and @tt{e} is the body of the function. For example, the function abstraction @tt{λx.x + 1} defines a function of @tt{x} that adds @tt{x} to @tt{1}. We say that @tt{λx.e} @emph{binds} the variable @tt{x} in @tt{e} and that @tt{e} is the @emph{scope} of the variable. Parentheses can be added to lambda expressions for clarity. Thus, we could have written this function abstraction as @tt{λx.(x + 1)}.

Note, we have minimized the syntax from the one used in Racket. This example does not illustrate the pure lambda calculus, because it uses the @racket[+] operator, which is not part of the pure lambda calculus; however, this example is easier to understand than a pure lambda calculus example.

A @emph{function application}, often called a lambda application, consists of an expression followed by an expression: @tt{e e}. The first expression is a function abstraction and the second expression is the argument to which the function is applied. All functions in lambda calculus have exactly one argument. For example, the lambda expression @tt{λx.x + 1 2} or written more clearly as @tt{λx.(x + 1) 2} is an application of the function @tt{λx. (x + 1)} to the argument @tt{2}.

Function application associates left-to-right; thus, @tt{f g h = (f g) h} and it also binds more tightly than @tt{λ}; thus, @tt{λx. f g x = (λx. (f g)x)}.

Functions in the lambda calculus are first-class citizens; that is to say, functions can be used as arguments to functions and functions can return functions as results.

@section{Currying / Multi-argument functions}

All functions in lambda calculus are prefix and take exactly one argument. So how do we represent functions that take multiple arguments?

If we want to apply a function to more than one argument, we can use a technique called currying that treats a function applied to more than one argument to a sequence of applications of one-argument functions.

For example, to express the sum of two numbers x and y we can write @tt{λx.λy.x + y}. To use this functions we have to apply it to two arguments in sequence. For example, to add 1 and 2, we use @tt{((λx.λy.x + y) 1) 2}. This is equivalent to a function in a programming language that allows multiple arguments. For example, in Racket and (our Lambda language syntax) the curried version looks like @racket[(((λ (x) (λ (y) (+ x y))) 1) 2)], which is equivalent to @racket[((λ (x y) (+ x y)) 1 2)].

@section{Free and Bound Variables}

In the function definition @tt{λx.x} the variable @tt{x} in the body of the definition (the second @tt{x}) is bound because its first occurrence in the definition is @tt{λx}. A variable that is not bound in expression @tt{e} is said to be free in @tt{e}. For example, in the function @tt{(λx.xy)}, the variable @tt{x} in the body of the function is bound and the variable @tt{y} is free.

Every variable in a lambda expression is either bound or free. Bound and free variables have quite a different status in functions.

@itemlist[
@item{In the expression @tt{(λx.x)(λy.yx)}:
@itemlist[
@item{The variable @tt{x} in the body of the leftmost expression is bound to the first lambda.}
@item{The variable @tt{y} in the body of the second expression is bound to the second lambda.}
@item{The variable @tt{x} in the body of the second expression is free.}
@item{Note that @tt{x} in second expression is independent of the @tt{x} in the first expression.}
]}
@item{In the expression @tt{(λx.xy)(λy.y)}:
@itemlist[
@item{The variable @tt{y} in the body of the leftmost expression is free.}
@item{The variable @tt{y} in the body of the second expression is bound to the second lambda.}
]}
]

We could check which variables are free using a @tt{free?} function that accepts a list of bound variables, the variable we want to check is free, and an expression to check:

@centered{@(scale (render-free?) 1.5)}

If the set of the bound variables is empty, a variable @tt{x1} is only free if the expression is only @tt{x1}. Otherwise the variable @tt{x1} is free if it is not contained in the set of bound variables and the expression is @tt{x1}. If the set of bound variables of empty, a variable @tt{x1} is free in @tt{λx2.e1} if @tt{x1} is free in @tt{e1} considering @tt{x2} as bound. Inductively, if the bound set is non-empty, the result is computed by including @tt{x2} in the set of bound variables. For function applications, @tt{x1} is free if it is free in either the function position @tt{e1} @emph{or} free in the argument position @tt{e2}.

@section{Alpha Reductions}

A variable may occur more than once in some lambda expression; some occurrences may be free and some may be bound, so the variable itself is both free and bound in the expression, but each individual occurrence is either free or bound (but not both). For example, the free variables of the following lambda expression are @tt{{y,x}} and the bound variables are @tt{{y}}:

@verbatim|{
        (λx.y)(λy.y x)
            |     | |
            |     | free
           free   |
                bound
}|

We need to distinguish between which @tt{y} is free and the one that is bound. Given a lambda expression @tt{(λx.e1) e2} instead of replacing all occurrences of @tt{x} in @tt{e1} with @tt{e2}, we replace all occurrences of @tt{x} that are free in @tt{e1} with @tt{e2}. For example:

@verbatim|{
               +----- e1 ----------+
               |                   |
           (λx. x + ((λx.x + 1) 3))  2
                |        |
                |        |
               free    bound
               in e1   in e1

             => 2 + ((λx.x + 1) 3)
}|

The issue here is that a variable @tt{x} that is free in the original argument to a lambda expression becomes bound after rewriting (using that argument to replace all instances of the formal parameter), because it is put into the scope of a lambda with a formal that happens also to be named @tt{x}:

@verbatim|{
    ((λy.λx. y) x) z
                |
                |
              free, but gets bound after application
}|

To solve this, we use a technique called @emph{alpha reduction}. The key idea is that formal parameter names are unimportant; so rename them as needed to avoid capture. Alpha reduction is used to modify expressions of the form @tt{λx.e}. It renames all the occurrences of @tt{x} that are free in @tt{e} to some other variable @tt{z} that does not occur in @tt{e} (and then @tt{λx} is changed to @tt{λz}). For example, consider @tt{λx.λy.x + y} (this is of the form @tt{λx.e}). Variable @tt{z} is not in @tt{e}, so we can rename @tt{x} to @tt{z}; i.e., @tt{λx.λy.x + y} alpha-reduces to @tt{λz.λy.z + y}.

Here is alpha reduction formally defined as the @tt{α} function. It takes the expression to be renamed, the variable to be renamed, and the new name of the variable. Crucially, the new name should be a fresh name, i.e., something that has not been used before.

@centered{@(scale (render-α) 1.5)}

α renames a variable @tt{x1} to @tt{x3} only if @tt{x1} is the same variable as the one to be renamed @tt{x2}. If the expression is a @tt{λ} where the binding argument is same variable being replaced, there is nothing to substituted. Otherwise, the body of the @tt{λ} is alpha renamed recursively. Function applications recursively call alpha reductions both in the function position and the argument position.

@section{Beta Reductions}

A function application @tt{λx.e y} is evaluated by substituting the argument @tt{y} for all free occurrences of the formal parameter @tt{x} in the body @tt{e} of the function definition @tt{λx.e}. This substitution is similar to the one presented in @secref{Let}. For example:

@itemlist[
@item{@tt{(λx.x) y} substitutes @tt{x = y} in the body @tt{x} and reduces to @tt{y}.}
@item{@tt{(λx.x z x) y} substitutes @tt{x = y} in the body @tt{x z x} and reduces to @tt{y z y}.}
@item{@tt{(λx.z) y} substitutes @tt{x = y} in the body @tt{z} and reduces to @tt{z} since the formal parameter @tt{x} does not appear in the body @tt{z}.}
]

This substitution in a function application is called a @emph{beta reduction}. If we use substitution to transform @tt{e1} to @tt{e2}, we say @tt{e1} reduces to @tt{e2} in one step. In general, @tt{(λx.e) y} means that applying the function @tt{(λx.e)} to the argument expression @tt{y} reduces by substituting @tt{x} with @tt{y} in @tt{e}, i.e., the argument expression @tt{y} is substituted for the function’s formal parameter @tt{x} in the function body @tt{e}.

A lambda calculus expression (aka a "program") is "run" by computing a final result by repeatedly applying beta reductions, until we cannot reduce it any further. It is the reflexive and transitive closure of the term under substitution, i.e., zero or more applications of beta reductions. For example:

@itemlist[
@item{@tt{(λx.x) y} reduces to @tt{y} (illustrating that @tt{λx.x} is the identity function).}
@item{@tt{(λx.x x) (λy.y)} reduces to @tt{(λy.y) (λy.y)} reduces to @tt{(λy.y)}; thus, we can say @tt{(λx.x x) (λy.y)} evaluates to @tt{(λy.y)}. Note that here we have applied a function to a function as an argument and the result is a function.}
]

Below is the behavior of beta reduction defined formally as the @tt{β} function. The first argument is a term in which variables will be substituted. The second argument is the variable whose occurrence will be substituted. Finally, the third argument is the term that will be substituted in places of the second argument.

@centered{@(scale (render-β) 1.5)}

β substitutes a variable with @tt{e} only if it is the same variable as the one to be substituted. If the expression is a lambda term and the variable to be substituted if rebound in the lambda term, it is returned as-is. Otherwise, if the rebound variable in the lambda is not free, the body of the lambda can be beta reduced recursively. However, if the rebound variable in the lambda is free, then we need a fresh variable name @tt{xt}, and alpha rename the lambda body followed by beta reduction of the lambda body. Finally, for function application, beta reduction is recursively doing beta reduction on the function position term and the argument position term.

@section{Meaning of a Lambda Expression}

A lambda calculus expression can be thought of as a program which can be executed by evaluating it. Evaluation is done by repeatedly finding a @emph{reducible expression} (called a @emph{redex}) and reducing it by a function evaluation until there are no more redexes.

@bold{Example 1:} The lambda expression @tt{(λx.x) y} in its entirety is a redex that reduces to @tt{y}.

@bold{Example 2:} The lambda expression @racket[(+ (* 1 2) (− 4 3))] has two redexes: @racket[(* 1 2)] and @racket[(− 4 3)]. If we choose to reduce the first redex, we get @racket[(+ 2 (− 4 3))]. We can then reduce @racket[(+ 2 (− 4 3))] to get @racket[(+ 2 1)]. Finally we can reduce @racket[(+ 2 1)] to get @tt{3}.

The term beta reduction is perhaps misleading, since doing beta reduction does not always produce a smaller lambda expression. In fact, a beta reduction can:

@itemlist[
@item{decrease,}
@item{increase,}
@item{not change}
]

the length of a lambda expression. Below are some examples. In the first example, the result of the beta-reduction is the same as the input (so the size doesn’t change); in the second example, the lambda expression gets longer and longer; and in the third example, the result first gets longer, and then gets shorter.

@itemlist[
@item{@tt{(λx.xx)(λx.xx)} reduces to @tt{(λx.xx)(λx.xx)}}
@item{@tt{(λx.xxx)(λx.xxx)} reduces to @tt{(λx.xxx)(λx.xxx)(λx.xxx)} reduces to @tt{(λx.xxx)(λx.xxx)(λx.xxx)(λx.xxx)}}
@item{@tt{(λx.xx)(λa.λb.bbb)} reduces to @tt{(λa.λb.bbb)(λa.λb.bbb)} reduces to @tt{λb.bbb}}
]

@section{Reduction Strategy}

A program can be reduced to find the resulting value. In other words, it takes a reducible expression, or redex, and reduces it using the reduction rules. However, here also multiple choices are available when reducing a term and a reduction strategy specifies the order in which beta reductions are made.

Some reduction orders for a lambda expression may yield a normal form while other orders may not. For example, consider the expression: @tt{(λx.z)((λx.x x)(λx.x x))}

This expression has two redexes:

@itemlist[
@item{The entire expression is a redex in which we can apply the function @tt{(λx.z)} to the argument @tt{((λx.x x)(λx.x x))} to yield the normal form @tt{z}. This redex is the leftmost outermost redex in the given expression.}
@item{The subexpression @tt{((λx.x x)(λx.x x))} is also a redex in which we can apply the function @tt{(λx.x x)} to the argument @tt{(λx.x x)}. Note that this redex is the leftmost innermost redex in the given expression. But if we evaluate this redex we get same subexpression: @tt{(λx.x x)(λx.x x)} → @tt{(λx.x x)(λx.x x)}. Thus, continuing to evaluate the leftmost innermost redex will not terminate and no normal form will result.}
]

As a second example, consider the expression: @tt{(λx. λy. y)((λz.z z)(λz.z z))}

This expression has two redexes:

@itemlist[
@item{The entire expression is a redex in which we apply the function @tt{(λx. λy. y)} to the argument @tt{((λz.z z)(λz.z z))} to yield the normal form @tt{(λy. y)}. This redex is the leftmost outermost redex in the given expression.}
@item{The subexpression @tt{((λz.z z)(λz.z z))} is also a redex in which we can apply the function @tt{(λz.z z)} to the argument @tt{(λz.z z)}. Note that this redex is the leftmost innermost redex in the given expression. But if we evaluate this redex we get same subexpression: @tt{((λz.z z)(λz.z z))} → @tt{((λz.z z)(λz.z z))}. Thus, continuing to evaluate the leftmost innermost redex will not terminate and no normal form will result.}
]

In general, there are two orders for evaluating lambda expressions:

@itemlist[
@item{@bold{Normal Order:} Replace bound variables by unevaluated argument expressions. The leftmost outermost redex is always reduced first and the arguments are substituted into the body before reduction. Often this is known as @emph{call-by-name}.

@centered{@(scale (render-cbn-rules-judgment) 1.5)}

Thus to give meaning to function application using call-by-name reduction semantics, we have to find the meaning of the function position term @tt{e1} which should be a lambda. Then argument position term @tt{e2} is substituted in the body of the lambda @tt{e3} in the place of @tt{x} using beta reduction which gives us the meaning of the entire function application.}
@item{@bold{Applicative order:} Broadly, replace the bound variables with the value of the argument expression. The leftmost innermost redex is always reduced first. Intuitively this means a function’s arguments are always reduced before the function itself. Often this is known as @emph{call-by-value}.

@centered{@(scale (render-cbv-rules-judgment) 1.5)}

Thus to give meaning to function application using call-by-value reduction semantics, we have to find the meaning of the function position term @tt{e1} which should be a lambda and the meaning of the argument @tt{e2} as @tt{v2}. Then @tt{v2} is substituted in the body of the lambda @tt{e3} in the place of @tt{x} using beta reduction which gives us the meaning of the entire function application.}
]

For @emph{pure programs}, i.e., programs without side effects, the call-by-name reduction strategy is a superset of call-by-value. If there exists a reduction to a value, call-by-name will find it, but the same may not hold true for call-by-value. Inversely, if call-by-value reduces a term to a value, a call-by-name evaluation will evaluate it to a value as well. If they run to completion, they will always reduce the same expression to the same value.

@section{Interpreter for Lambda Calculus}

To define the interpreter for the lambda calculus, we first need to translate the definitions of @tt{free?}, @tt{α}, and @tt{β} to code. All individual cases in in those above definitions become pattern matches and when we need the fresh variable name in the beta reduction we use the Racket builtin function @racket[gensym] to get us a fresh symbol name each time:

@codeblock-include["lambda/interp.rkt"]

We can run these functions to see if they behave as expected:

@#reader scribble/comment-reader
(examples #:eval ev
(free? '()  'x '(x ((λ (x) x) y)))
(free? '(x) 'x '(x ((λ (x) x) y)))
(free? '()  'x '(z ((λ (x) x) y)))

(alpha-reduce '(λ (y) x) 'x 'z)
(alpha-reduce '(λ (y) x) 'y (gensym))
(alpha-reduce '(λ (y) x) 'x (gensym))

(beta-reduce '(x x) 'x '(λ (x) (x x)))
(beta-reduce '(λ (y) x) 'x 'y)
)

@;{#reader scribble/comment-reader
(examples #:eval ev
(check-true  (free? '()  'x '(x ((λ (x) x) y))))
(check-false (free? '(x) 'x '(x ((λ (x) x) y))))
(check-false (free? '()  'x '(z ((λ (x) x) y))))

(check-equal? (alpha-reduce '(λ (y) x) 'x 'z)
            '(λ (y) z))
(check-equal? (alpha-reduce '(λ (y) x) 'y (gensym))
            '(λ (y) x))
(check-match  (alpha-reduce '(λ (y) x) 'x (gensym))
            `(λ (y) ,(? symbol?)))

(check-equal? (beta-reduce '(x x) 'x '(λ (x) (x x)))
            '((λ (x) (x x)) (λ (x) (x x))))
(check-match (beta-reduce '(λ (y) x) 'x 'y)
            `(λ (,(? symbol?)) y))
)}

Now, all we have to do is decide a reduction strategy and build an interpreter.

@subsection{Call-by-name}

A call-by-name interpreter only reduces the function position term before beta-reduction and then interprets the reduced term again.

@#reader scribble/comment-reader
(examples #:eval ev
; call-by-name
; Expr -> Val
(define (interp-cbn e)
  (match e
    [`(,e1 ,e2) (match (interp-cbn e1)
                  [`(λ (,x) ,e) (interp-cbn (beta-reduce e x e2))]
                  [_            (error "can only apply functions")])]
    [_ e]))
)

Let us see an example of two terms that evaluates to a value using call-by-name:

@#reader scribble/comment-reader
(examples #:eval ev
(interp-cbn '((λ (x) z) (λ (x) x)))
(interp-cbn '((λ (x) z) ((λ (x) (x x)) (λ (x) (x x)))))
)

@subsection{Call-by-value}

A call-by-name interpreter reduces both the function position term and the argument position term before beta-reduction and then interprets the reduced term again.

@#reader scribble/comment-reader
(examples #:eval ev-time
; call-by-value
; Expr -> Val
(define (interp-cbv e)
  (match e
    [`(,e1 ,e2) (match (interp-cbv e1)
                  [`(λ (,x) ,e) (interp-cbv (beta-reduce e x (interp-cbv e2)))]
                  [_            (error "can only apply functions")])]
    [_ e]))
)

Let us try to run the same two terms using call-by-value again:

@#reader scribble/comment-reader
(examples #:eval ev-time
(interp-cbv '((λ (x) z) (λ (x) x)))
(eval:error (interp-cbv '((λ (x) z) ((λ (x) (x x)) (λ (x) (x x))))))
)

Notice how the second example reduces successfully using call-by-name but gets stuck using call-by-value reduction?

@subsection{Intermediate Steps}

To see how call-by-name differs from call-by-value, we can look at the intermediate steps of the reduction. For that all we have to do is remove the recursive call to the interpreter around our beta reduction.

For call-by-name, it looks like:

@#reader scribble/comment-reader
(examples #:eval ev2
(define (interp-cbn-step e)
  (match e
    [`(* ,ea1 ,ea2)          (* (interp-cbn-step ea1) (interp-cbn-step ea2))]
    [`(,e1 ,e2)              (match (interp-cbn-step e1)
                               [`(λ (,x) ,e) (beta-reduce e x e2)]
                               [(? symbol? x) x])]
    [_                       e]))
)

Notice how we added a case for @racket[*]? It allows us to write more interesting programs that do arithmetic:

@#reader scribble/comment-reader
(examples #:eval ev2
(interp-cbn-step '((λ (x) (* x x)) (* 2 3)))
)

Thus, the expression @racket[(* 2 3)] is substituted directly. We can continue running until we get the value:

@#reader scribble/comment-reader
(examples #:eval ev2
(interp-cbn-step '(* (* 2 3) (* 2 3)))
)

Let us contrast this to a call-by-value interpreter that shows the intermediate steps:

@#reader scribble/comment-reader
(examples #:eval ev2
(define (interp-cbv-step e)
  (match e
    [`(* ,ea1 ,ea2)          (* (interp-cbv-step ea1) (interp-cbv-step ea2))]
    [`(,e1 ,e2)              (match (interp-cbv-step e1)
                               [`(λ (,x) ,e) (beta-reduce e x (interp-cbv-step e2))]
                               [(? symbol? x) x])]
    [_                       e]))
)

Let us run the same expression again:

@#reader scribble/comment-reader
(examples #:eval ev2
(interp-cbv-step '((λ (x) (* x x)) (* 2 3)))
)

Thus, the expression @racket[(* 2 3)] is evaluated and then substituted. We can continue running until we get the value:

@#reader scribble/comment-reader
(examples #:eval ev2
(interp-cbn-step '(* 6 6))
)

@section{Adding Lambdas to our Language}

We can now take these core ideas and implement this in our existing interpreter. Below is an implementation that shows how it looks when our language has integers, arithmetic operations like @racket[*] and @racket[-], @racket[if], and @racket[let]. You can try extending these definitions for the rest of the language on your own. We are using call-by-name semantics below, but you can changing the reduction semantics on your own to see how the behavior changes.

@codeblock-include["lambda/interp-full.rkt"]

Most operations push @tt{free?}, @tt{alpha-reduce}, @tt{beta-reduce}, and @tt{interp} to the inner terms. The only exception is @racket[let]: we have to be careful to handle the changes it makes to the variable bindings.

Let us use this interpreter to encode different language features just using lambdas to show you that lambdas are all you need!

@section{Encoding programs in Lambda Calculus}

So far we look at how we can compute with lambda expressions. Turns out we can encode any behavior of a computable program in a lambda expression. This means we can encode all language features we developed until now (or any feature you want from your favorite language) as lambda expressions!

@subsection{Let Bindings}

Let us recall what a let binding like @racket[(let ((x e1)) e2)] did: it bound @tt{e1} to @tt{x}, and made @tt{x} a bound variable in the expression @tt{e2}. This is infact what lambdas do as well.

A lambda term @tt{((λx.e2) e1)} is exactly the same — it evaluates to @tt{e2}, while binding @tt{x} to @tt{e1}. We can actually run this through our interpreter to confirm it is correct:

@#reader scribble/comment-reader
(examples #:eval ev2
(interp '(let ((x 5))
             (- x 1)))
(interp '((λ (x) (- x 1)) 5))
)

@subsection{Conditionals and Boolean Values}

First, let’s consider how to encode a conditional expression: @tt{(if p a b)} (i.e., the value of the whole expression is either @tt{a} or @tt{b}, depending on the value of @tt{p}). We will represent this conditional expression using a lambda expression of the form: @tt{(cond p a b)}, where @tt{cond}, @tt{p}, @tt{a} and @tt{b} are all lambda expressions. In particular, @tt{cond} is a function of 3 arguments that works by applying @tt{p} to @tt{a} and @tt{b} (i.e., @tt{p} is the condition and @tt{a} is the then-branch and @tt{b} is the else branch):

@tt{cond  ::= λp.λa.λb.p a b}

To make this definition work correctly, we must define the representations of true and false carefully (since the lambda expression @tt{p} that cond applies to its arguments @tt{a} and @tt{b} will reduce to either true or false). In particular, when true is applied to @tt{a} and @tt{b} we want it to return @tt{a}, and when false is applied to @tt{a} and @tt{b} we want it to return @tt{b}. Therefore, we will let true be a function of two arguments that ignores the second argument and returns the first argument, and we’ll let false be a function of two arguments that ignores the first argument and returns the second argument:

@tt{true  ::= λx.λy.x}

@tt{false ::= λx.λy.y}

Now let’s consider an example: @tt{cond true m n}. Note that this expression should evaluate to @tt{m}. Let’s see if it does (by substituting our definitions for @tt{cond} and @tt{true}, and evaluating the resulting expression). The sequence of beta reductions is shown below; in each case, the redex about to be reduced is indicated by underlining the formal parameter and the argument that will be substituted in for that parameter.

@verbatim|{
   (λp.λa.λb.p a b)(λx.λy.x) m n
=> (λa.λb.(λx.λy.x)a b) m n
=> (λb.(λx.λy.x) m b) n
=> (λx.λy.x) m n
=> (λy.m)n
=> m
}|

We can run such programs through Racket and confirm our understanding is correct:

@#reader scribble/comment-reader
(examples #:eval ev2
(interp '(let ((true  (λ (x) (λ (y) x))))
             (let ((false (λ (x) (λ (y) y))))
               (let ((cond  (λ (c) (λ (t) (λ (e) ((c t) e))))))
                 (((cond false) 4) 5)))))
(interp '(let ((true  (λ (x) (λ (y) x))))
             (let ((false (λ (x) (λ (y) y))))
               (let ((cond  (λ (c) (λ (t) (λ (e) ((c t) e))))))
                 (((cond true) 4) 5)))))
)

We can go and replace all the @racket[let]-s to encode this purely using lambdas. It is not very readable, but it is the same program:

@#reader scribble/comment-reader
(examples #:eval ev2
(interp '((((λ (c) (λ (t) (λ (e) ((c t) e)))) (λ (x) (λ (y) y))) 4) 5))
(interp '((((λ (c) (λ (t) (λ (e) ((c t) e)))) (λ (x) (λ (y) x))) 4) 5))
)

@;subsection{Integers}

@section{Y-Combinator}

We have one final issue to deal with: recursive functions. For example, consider how we can write the factorial function in Racket:

@#reader scribble/comment-reader
(examples #:eval ev
(define (fact n)
  (if (zero? n) 1
      (* n (fact (- n 1)))))
(fact 5)
)

This crucially relies on the ability to name our functions. Without the ability to call the function @tt{fact}, how do we recursively call it? If adding lambdas to our language makes it truly a general computing language we should be able to do recursion.

To do this we use the Y-combinator in lambda calculus which is defined as:

@tt{λf.(λy.f(y y))(λy.f(y y))}

This computes the fix-point of a function in call-by-name reduction semantics, i.e., the result of applying the function repeatedly until it terminates. Assuming you are applying the the Y-combinator to a function @tt{g}:

@verbatim|{
   (Y g)
=> (λf.(λy.f(y y))(λy.f(y y)) g)
=> (λy.g(y y)) (λy.g(y y))
=> g((λy.g(y y)) (λy.g(y y)))
=> g (Y g)
=> ...
}|

Thus the Y-combinator continues running @tt{g} until @tt{g} reaches a fix-point, i.e., terminates.

Let us write the factorial function using the Y-combinator:

@#reader scribble/comment-reader
(examples #:eval ev2
(interp '(let ((Y (λ (f) ((λ (x) (f (x x))) (λ (x) (f (x x)))))))
             (let ((fact (Y (λ (f)
                              (λ (n)
                                (if (zero? n) 1
                                    (* n (f (- n 1)))))))))
               (fact 5))))
)

The definition of @tt{fact} changes to accept a lambda that is called recursively from the function body. Calling @racket[(fact 5)] produces the factorial of @racket[5], @racket[120].

We could get rid of the @racket[let]-s and use pure lambdas:

@#reader scribble/comment-reader
(examples #:eval ev2
(interp '(((λ (f) ((λ (x) (f (x x))) (λ (x) (f (x x)))))
             (λ (fact)
               (λ (n)
                 (if (zero? n) 1
                     (* n (fact (- n 1))))))) 5))
)

@section{Discussion}

We should take a pause, and appreciate the milestone we have reached. Our Lambda language implements an elaborate version of the Lambda Calculus—a general purpose programming language. Thus, the language Lambda can represent all possible computation one can ever represent in any general purpose language (including C, C++, Java, Ruby, Python, JavaScript, Haskell).

If our language can express all kinds of programs, we are done, right? What should we do for the latter half of the semester? Think on it!
