#lang scribble/manual

@(require "../fancyverb.rkt" "../utils.rkt")
@(require redex pict "model.rkt")
@(require (for-label racket rackunit))
@(require scribble/examples racket/sandbox)

@(define codeblock-include (make-codeblock-include #'here))

@(ev '(require rackunit))
@(for-each (λ (f) (ev `(require (file ,(path->string (build-path "examples" "types" f))))))
	   '("types.rkt"))

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

@title[#:tag "Types"]{Type System}

@table-of-contents[]

@section{Checking Program Invariants Statically}

As programs grow larger or more subtle, developers need tools to help them
describe and validate statements about program invariants. Invariants, as the
name suggests, are statements about program elements that are expected to always
hold of those elements. For example, if we write @racket[x : number] in our (to be
developed) typed language, we mean that @tt{x} will always hold a number, and that
all parts of the program that depend on @tt{x} can rely on this statement being
enforced. As we will see, types are just one point in a spectrum of invariants
we might wish to state, and static type checking—itself a diverse family of
techniques—is also a point in a spectrum of methods we can use to enforce the
invariants.

In this module, we will focus especially on @emph{static type checking}: that
is, checking (declared) types before the program even executes. You might be
familiar with this from your prior use of Haskell or Java. We will explore some
of the design space of types and their trade-offs and learn why static typing is
an especially powerful and important form of invariant enforcement.

Consider the following program in a typed version of our language. We will
define the full language later, but the example should be simple to understand.

@racketblock[
(let ((f (λ (n : int) : int
           (+ n 3))))
  (f #t))
]

Our type system should be able to flag the type error before the program begins
execution. The same program (without the type annotations) in ordinary Racket
fails only at runtime:

@#reader scribble/comment-reader
(examples #:eval ev
(eval:error (let ((f (λ (n)
           (+ n 3))))
  (f #t)))
)

But before we define our typed language, we will have to understand what are
types.

@section{Syntax}

The general syntax of the typed language is shown as below. It is same as the
language with @racket[lambda] functions, except for places where new variables are
introduced. So lambda function have type annotations attached to each argument
and finally another type to denote the type of value it produces. The other
place we need types are @racket[let] bindings.

@centered{@(scale (render-types-lang) 1.5)}

Before we proceed we still have to describe what types (the @(render-term L T) in the above
figure) are. Types can be base types for constants like @(tt "Int") and @(tt "Bool") in our
language. They can also be function types denoted by the arrow. The last type in
the function type is the type of value computed by the function and
anything preceding it are the arguments. Thus, @(tt "Int -> Bool") is a function
that takes an integer and produces a boolean, while @(tt "Int -> Int -> Int") is a
function that takes two integer arguments and produces an
integer.

@section{Type Checker}

Now that we’ve fixed both the term and type structure of the language, let’s
make sure we agree on what constitute type errors in our language (and, by extension,
everything not a type error must pass the type checker). Here are a few obvious
forms of type errors:

@itemlist[
@item{One or both arguments of @racket[+] is not an integer, i.e., is not a
@tt{Int}.}
@item{One or both arguments of @racket[*] is not an integer.}
@item{The expression in the function position of an application is not a
function, i.e., is not a @tt{->}.}
@item{The expression in the function position of an application is a function
but the type of the actual argument does not match the type of the formal
argument expected by the function.}
]

A natural starting signature for the type-checker would be that it is a
procedure that consumes an expression and returns a type for it. If it cannot,
it will throw a type error. Because we know expressions contain identifiers, it
soon becomes clear that we will want a type environment @(render-term L Γ),
which maps names to types, analogous to the value environment we have seen so
far.

Just like we did with environments, we define a store function that given a type
environment, a variable, and a value to be stored, it extends the environment
and returns this @emph{new} environment.

@centered{@(scale (render-store) 1.5)}

Additionally, we define a lookup function that given a type environment and a
variable looks up the the environment recursively and returns the type bound to
that variable.

@centered{@(scale (render-lookup) 1.5)}

We can give semantics for a type-correct program as below:

@itemlist[
@item{@emph{Integers} always have the type @tt{Int}}
@item{@emph{Booleans} always have the type @tt{Bool}}
@item{An expression of the form @racket[(add1 e)] or @racket[(sub1 e)] is of
type @tt{Int} if @tt{e} is of type @tt{Int}}
@item{An expression of the form @racket[(+ e1 e2)], @racket[(- e1 e2)],
@racket[(* e1 e2)], and @racket[(/ e1 e2)] is of type @tt{Int} if @tt{e1} and
@tt{e2} both are of type @tt{Int}}
@item{An expression of the form @racket[(zero? e)] is of type @tt{Bool} if
@tt{e} is of type @tt{Int}}
@item{An expression of the form @racket[(<= e1 e2)] is of type @tt{Bool} if
@tt{e1} and @tt{e2} both are of type @tt{Int}}
@item{An expression of the form @racket[(and e1 e2)] has the same type as
@tt{e1} and @tt{e2} if both @tt{e1} and @tt{e2} has the same type}
@item{An expression of the form @racket[(if e1 e2 e3)] has the same type as
@tt{e2} and @tt{e3} if both @tt{e2} and @tt{e3} has the same type and @tt{e1} is
a @tt{Bool}}
]

@centered{@(scale (render-types-rules-judgment-1) 1.5)}

The judgment here says in the type environment @tt{Γ}, the expression @tt{e} has
a type @tt{T}. The type of an expression always follows the @tt{:}.

Notice the type for @racket[and]: we are now defining that both operands need to be
of same type @tt{T} and @racket[and] will produce a boolean. This was not in fact
how we defined @racket[and] in our interpreter! Our underlying language can
handle all types of arguments (both integers and booleans at the same time)
passed to @racket[and]. However, by restricting the types to be the same type,
we are restricting the behavior of the language. This is an important
characteristic: type systems can change the semantics of the language and our
interpreter should behave soundly with respect to the interpreter.

Now we have to define the rules for variables and function abstraction and
application.

@itemlist[
@item{A lambda expression of the form @racket[(λ (x : T1) : T2 e)] is of
type @tt{(-> T1 T2)} if if body of the lambda @tt{e} can be type checked to have
type @tt{T2} under the extended type environment with @tt{x} bound to @tt{T1}}
@item{A variable has the same type as what it is bound to in the type
environment @tt{Γ}}
@item{A @racket[let] binding of the form @racket[(let ((x : T e1)) e2)] has the
same type as @tt{e2} if @tt{e1} can be type checked to the type @tt{T} and
@tt{e2} can be typechecked under the extended environment with @tt{x} bound to
@tt{T}}
@item{Function applications of the form @racket[(e1 e2)] expects the first term
to be the function, i.e., have a function type. All other terms are checked to
have correct argument types, in which case the entire application expression is
given the return type of the function}
]

@centered{@(scale (render-types-rules-judgment-2) 1.5)}

@section{Implementing the Type Checker}

Just like our interpreter, the type checker pattern matches on each form of the
language and type checks it. In our implementation we use @tt{TE} to denote the
type environment. Just like our interpreter, any type derivation in the premise
becomes a recursive call. We implement these in the @tt{tc} function below:

@codeblock-include["types/types.rkt"]

After this, you might wonder type checking and interpretation are very similar,
but there are some key differences. For example, from the perspective of
type-checking (in this type language), there is no difference between binary
operations like @racket[+], @racket[-], @racket[*], and @racket[/], or indeed
between any two functions that 
consume two numbers and return one. For such operations, observe another
difference between interpreting and type-checking. Arithmetic operations care
that the arguments be numbers. The interpreter then returns a precise sum or
product, but the type-checker is indifferent to the differences between them:
therefore the expression that computes what it returns @tt{Int} is a constant, and
the same constant in both cases. 

Observe another curious difference between the interpreter and type-checker. In
the interpreter, application was responsible for evaluating the argument
expression, extending the environment, and evaluating the body. Here, the
function application case does check the argument expression, but leaves the
environment alone, and simply returns the type of the body without traversing
it. Instead, the body is actually traversed by the checker when checking a
function definition, so this is the point at which the environment actually
extends.

@section{Type Checking Conditionals}

Although we presented the typing rule and implementation for @racket[if], it
introduces several design decisions. We’ll discuss two here:

@itemlist[
@item{What should be the type of the condition expression? In some languages it must
evaluate to a boolean value, while in other languages it can be any value, and
some values are considered “truthy” while others “falsy”.}
@item{What should be the relationship between the then- and else-branches? In
some languages they must be of the same type, so that there is a single,
unambiguous type for the overall expression (which is that one type). In other
languages the two branches can have distinct types, which greatly changes the
design of the type-language and -checker, but also of the nature of the
programming language itself.}
]

This can lead to a variety of rules in our language. The rule we already
presented says the condition expression will always have to be a boolean, and
both branches have to be the same type.

This is followed in Java, but it can turn out to be restricting for type systems
for programs that return values of different types on different branches. A
popular solution is to extend the language of types to add @emph{union types}. This
involves adding a form @tt{T U T} to the syntax of types.

Union types denote a value can be either of type @tt{T1} or @tt{T2}, and there is no
way to tell statically which one it will be be. Then we can define @tt{union} as an
operation that given two distinct types can merge them into an union type and
handles @racket[if] with branches producing different types.

@centered{@(scale (render-types-rules-judgment-3) 1.5)}

This would correspond to updating the case for @racket[if] in our type checker:

@racketblock[
[`(if ,e1 ,e2 ,e3)          (match* ((tc TE e1) (tc TE e2) (tc TE e3))
                                  [(_  t2 t3) `(U ,t2 ,t3)]
                                  [(_  _  _ ) (error "type error: if")])]
]

Thus something as simple as type checking `if` can involve a lot of decision of
about the language semantics and the type system.

@section{Recursion}

Now that we’ve obtained a basic programming language, let’s add recursion to it.
The simplest recursive program is, of course, one that loops forever. Can we
write an infinite loop with just functions? We already could simply with this
program:

@racketblock[
((λ (x) (x x))
 (λ (x) (x x)))
]

which we know we can represent in our language with functions as values.

But now that we have a typed language, and one that forces us to annotate all
functions, let’s annotate it. The first subexpression is clearly a function
type, and the function takes one argument, so it must be of the form @tt{T1 -> T2}.
Now what is that argument @tt{T1}? It is the type itself. Thus, the type of the
function @tt{T1 -> T2} which expands into @tt{(T1 -> T2) -> T2}, which further expands
to @tt{((T1 -> T2) -> T2) -> T2}, and so on. In other words, this type cannot be
written as any finite string! If we cannot write the type, we will never be able
to expression this program in our typed language in the first place. 

@bold{Program Termination:} The typed language we have so far has a property
called @emph{strong normalization}: every expression that has a type will terminate
computation after a finite number of steps. In other words, this special (and
peculiar) infinite loop program isn’t the only one we can’t type; we can’t type
@emph{any} infinite loop (or even potential infinite loop). A rough intuition that
might help is that any type---which must be a finite string---can have only a finite
number of @tt{->}’s in it, and each application discharges one, so we can perform
only a finite number of applications. 

If our language permitted only straight-line programs, this would be
unsurprising. However, we have conditionals and even functions being passed
around as values, and with those we can encode any datatype we want. Yet, we
still get this guarantee! That makes this a somewhat astonishing result.

This result also says something deeper. It shows that, contrary to what you may
believe—that a type system only prevents a few buggy programs from running - a
type system can @emph{change the semantics} of a language. Whereas previously we
could write an infinite loop in just one to two lines, now we cannot write one
at all. It also shows that the type system can establish invariants not just
about a particular program, but @emph{about the language itself}. If we want to
absolutely ensure that a program will terminate, we simply need to write it in
this language and pass the type checker, and the guarantee is ours!

@;{margin-note{These are not hypothetical examples. In the Standard ML language,
the language for linking modules uses essentially this typed language for
writing module linking specifications. This means developers can write quite
sophisticated abstractions—they have functions-as-values, after all!—while still
being guaranteed that linking will always terminate, producing a program.}}

What possible use is a language in which all programs terminate? For
general-purpose programming, none, of course. But in many specialized domains,
it’s a tremendously useful guarantee to have. For instance, suppose you are
implementing a complex scheduling algorithm; you would like to know that your
scheduler is guaranteed to terminate so that the tasks being scheduled will
actually run. There are many other domains, too, where we would benefit from
such a guarantee: a packet-filter in a router; a real-time event processor; a
device initializer; a configuration file; the callbacks in single-threaded
JavaScript; and even a compiler or linker. In each case, we have an almost
unstated expectation that these programs will always terminate. And now we have
a language that can offer this guarantee—something it is impossible to test for,
no less! 

@bold{Typing Recursion:} What this says is, now we must make recursion an
explicit part of the typed language with form @racket[letrec].

@racketblock[
(letrec ((fact : (-> Int Int) (λ (n : Int) : Int
                                (if (zero? n) 1
                                    (* n (fact (- n 1)))))))
  (fact 5))
]

for the factorial function @tt{fact}, where @tt{n} is its argument, and @tt{Int}
the type consumed by and returned from the function. The expression
@racket[(fact 5)] represents the use of this function. 

How do we type such an expression? Clearly, we must have @tt{n} bound in the body
of the function as we type it (but not of course, in the use of the function);
this much we know from typing functions. But what about @tt{fact}? Obviously it
must be bound in the type environment when checking the use (@racket[(fact 5)]), and
its type must be @tt{Int -> Int}. But it must also be bound, to the same type, when
checking the body of the function. (Observe, too, that the type returned by the
body must match its declared return type.)

@centered{@(scale (render-types-rules-judgment-4) 1.5)}

Now we can see how to break the shackles of the finiteness of the type. It is
certainly true that we can write only a finite number of @tt{->}s in types in the
program source. However, this rule for typing recursion duplicates the @tt{->} in
the body that refers to itself, thereby ensuring that there is an inexhaustible
supply of applications. It’s our infinite quiver of arrows.

The code to implement this rule would be another line in the pattern match:

@racketblock[
[`(letrec ((,x : ,T ,e1)) ,e2)  (if (equal? (tc (store TE x T) e1) T)
                                    (tc (store TE x T) e2)
                                    (error "binder got wrong type"))]
]

@;{section{Types and Runtime}}

@section{Type Soundness}

We have seen earlier that certain type languages can offer very strong theorems
about their programs: for instance, that all programs in the language terminate.
In general, of course, we cannot obtain such a guarantee (indeed, we added
general recursion precisely to let ourselves write unbounded loops). However, a
meaningful type system—indeed, anything to which we wish to bestow the noble
title of a type system—ought to provide some kind of meaningful guarantee that
all typed programs enjoy. This is the payoff for the programmer: by typing this
program, she can be certain that certain bad things will certainly not happen.
Short of this, we have just a bug-finder; while it may be useful, it is not a
sufficient basis for building any higher-level tools (e.g., for obtaining
security or privacy or robustness guarantees).

What theorem might we want of a type system? Remember that the type checker runs
over the static program, before execution. In doing so, it is essentially making
a prediction about the program’s behavior: for instance, when it states that a
particular complex term has type @tt{Int}, it is effectively predicting that when
run, that term will produce an integer value. How do we know this prediction is
sound, i.e., that the type checker never lies? Every type system should be
accompanied by a theorem that proves this.

There is a good reason to be suspicious of a type system, beyond general
skepticism. There are many differences between the way a type checker and a
program evaluator work:

@itemlist[
@item{The type checker only sees program text, whereas the interpreter runs over actual stores.}
@item{The type environment binds identifiers to types, whereas the interpreter’s environment binds identifiers to values or locations.}
@item{The type checker compresses (even infinite) sets of values into types, whereas the interpreter treats these distinctly.}
@item{The type checker always terminates, whereas the interpreter might not.}
@item{The type checker passes over the body of each expression only once, whereas the evaluator might pass over each body anywhere from zero to infinite times.}
]

Thus, we should not assume that these will always correspond!

The central result we wish to have for a given type-system is called
@emph{soundness}. It says: suppose we are given an expression (or program) @tt{e}.
We type-check it and conclude that its type is @tt{T}. When we run @tt{e}, let us say
we obtain the value @tt{v}. Then @tt{v} will also have type @tt{T}.

The standard way of proving this theorem is to prove it in two parts, known as
@emph{progress} and @emph{preservation}. Progress says that if a term passes the
type-checker, it will be able to make a step of evaluation (unless it is already
a value); preservation says that the result of this step will have the same type
as the original. If we interleave these steps (first progress, then
preservation; repeat), we can conclude that the final answer will indeed have
the same type as the original, so the type system is indeed sound.

For instance, consider this expression: @racket[(+ 5 (* 2 3))]. It has the type
@tt{Int}. In a sound type system, progress offers a proof that, because this
term types, and is not already a value, it can take a step of execution—which it
clearly can. After one step the program reduces to @racket[(+ 5 6)]. Sure
enough, as preservation proves, this has the same type as the original:
@tt{Int}. Progress again says this can take a step, producing @racket[11].
Preservation again shows that this has the same type as the previous
(intermediate) expressions: @tt{Int}. Now progress finds that we are at an
answer, so there are no steps left to be taken, and our answer is of the same
type as that given for the original expression.

However, this isn’t the entire story. There are two caveats:

@itemlist[#:style 'ordered
@item{The program may not produce an answer at all; it might loop forever. In
this case, the theorem strictly speaking does not apply. However, we can still
observe that every intermediate term still has the same type, so the program is
computing meaningfully even if it isn’t producing a value.}
@item{Any rich enough language has properties that cannot be decided statically
(and others that perhaps could be, but the language designer chose to put off
until run-time). When one of these properties fails---e.g., the array index being
within bounds—there is no meaningful type for the program. Thus, implicit in
every type soundness theorem is some set of published, permitted exceptions or
error conditions that may occur. The developer who uses a type system implicitly
signs on to accepting this set.}
]

As an example of the latter set, the user of a typical typed language
acknowledges that vector dereference, list indexing, and so on may all yield
exceptions.

The latter caveat looks like a cop-out. In fact, it is easy to forget that it is
really a statement about what cannot happen at run-time: any exception not in
this set will provably not be raised. Of course, in languages designed with
static types in the first place, it is not clear (except by loose analogy) what
these exceptions might be, because there would be no need to define them. But
when we retrofit a type system onto an existing programming language—especially
languages with only dynamic enforcement, such as Racket or Python—then there is
already a well-defined set of exceptions, and the type-checker is explicitly
stating that some set of those exceptions (such as “non-function found in
application position” or “method not found”) will simply never occur. This is
therefore the payoff that the programmer receives in return for accepting the
type system’s syntactic restrictions.

@emph{These notes are adapted from @link["https://cs.brown.edu/courses/cs173/2012/book/types.html"]{CS173 at Brown}.}
