#lang scribble/manual

@(require "../notes/fancyverb.rkt" "../notes/utils.rkt")
@(require (for-label racket rackunit))
@(require scribble/examples racket/sandbox)

@title[#:tag "A5"]{A5: Type System}

@bold{Due:} May 7, 2025

You are given a file on Canvas @tt{types.rkt} with a starter code for the
assignment.

The goal of this assignment is to extend the type system we discussed in class
to the language with state. With this change, you will extend your type system
to go from a functional language to an imperative one.

More concretely, the type checker implemented in the starter code is for the
language from @secref{Lambda}. Extend this type checker to support the language
from @secref{State}. Specifically, you have to extend the type checker to
support the @tt{seq}, ways to allocate, access, deallocate, and update state via
@tt{new}, @tt{free}, @tt{deref}, and @tt{set!} respectively by adding references
to the type system. Your goal in this assignment is add support for references
in the type system.

References in this type system is denoted by the @tt{Ref} type. However, just
denoting references with a base type @tt{Ref} does not tell was anything about
the underlying data type that the reference points to. For example, if @tt{x} is a
reference we have no idea what kind of value @tt{(deref x)} will return. This can
cause unsoundness in the type system! To mitigate this we have to track what
type of values references point to. To use this we will use parametric
polymorphism where the base type will be @tt{Ref}, parametrized by the type of
value it is referencing. For example, @tt{(new 5)} will have a type @tt{(Ref Int)},
@tt{(new #t)} will have a type @tt{(Ref Bool)}. Dereferencing will then give us
back the expected type: @tt{(deref (new 5))} will be of type @tt{Int}.

These parameterized @tt{Ref} types are represented using our familiar list
notation:

@racketblock[
'(Ref Int)
]

@section{Type @tt{seq}}

@tt{seq} is used to define a sequence of expressions in the program. When a
sequence of expressions are type checked, all expressions are individually type
checked, but the entire sequence has the type of the last expression. For
example, the following expression has the type @racket['Bool].

@racketblock[
(seq
  (let ((x : Int (+ 5 4)))
    (+ x 2))
  #t)
]

@section{Type @tt{new}}

@tt{new} is used to allocate a value on to the program state. The type of a
@tt{(new e)} is the reference to the type of @tt{e}. For
example, the following expression has the type @racket['(Ref (-> Int Int))].

@racketblock[
(new (λ (x : Int) : Int x))
]

@section{Type @tt{deref}}

@tt{deref} is used to dereference a reference to the program state. The type of
a @tt{(deref e)} is the dereferenced type of @tt{e}. It is a type error is `e`
is not a reference. The following expression has the type @racket['(-> Int Int)].

@racketblock[
(deref (new (λ (x : Int) : Int x)))
]

@section{Type @tt{set}}

@tt{set} is used to mutate value in the program state and returns the updated
value. The type of a @tt{(set e1 e2)} is the type @tt{e2} if @tt{e1} has a
refers to the same type as @tt{e2}. This means once a location has been created
in the state with a particular type of value, it can only store values of the
same type in future. 

For example, the following has the type @racket['Int]. The @racket[(new 4)]
expression has type @racket['(Ref Int)], so @tt{set} will allow update of
@racket['(Ref Int)] with an @racket['Int].

@racketblock[
(set (new 4) 5)
]

However, the following results in a type error. @tt{set} should now allow update
of location of type @racket['(Ref Int)] with a @racket['Bool] value.

@racketblock[
(set (new 4) #t)
]

@tt{set} is also type error if @tt{e1} does not have a reference type.

@section{Type @tt{free}}

@tt{free} is used to free a value stored in a location and returns the location.
The type of a @tt{(free e)} is the type @tt{e}. It is a type error if @tt{e}
does not have a reference type. The following expression has the type
@racket['(Ref Int)].

@racketblock[
(free (new 4))
]

@section{Testing}

You should test your code by writing test cases and adding them to relevant
files. The starter code is available in @tt{types.rkt} on Canvas. Use the command
@tt{raco test [filename]} to test your code. Alternatively, pressing “Run” in
Dr. Racket will also run your test cases.

For grading, your submitted type checker will be tested on multiple programs
drawn from this language. Writing your own test cases will give you confidence
that your type checker can handle previously unseen programs.

@section{Submitting}

You should submit on Gradescope. You should submit the file @tt{types.rkt} for
grading, so make sure all your work is contained there! You may add any function
you need to these files, but do not rename the file or the @tt{tc} function.
