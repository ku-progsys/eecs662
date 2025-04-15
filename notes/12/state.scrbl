#lang scribble/manual

@(require "../fancyverb.rkt" "../utils.rkt")
@(require redex pict "model.rkt")
@(require (for-label racket rackunit))
@(require scribble/examples racket/sandbox)

@(define codeblock-include (make-codeblock-include #'here))

@(ev '(require rackunit))
@(for-each (λ (f) (ev `(require (file ,(path->string (build-path "examples" "state" f))))))
	   '("interp.rkt"))
@(ev '(define (eval e)
  (let ((E '())
        (S (make-hash)))
    (interp E S e))))

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

@title[#:tag "State"]{Program State}

@table-of-contents[]

@section{Imperative Languages}

Until this point in the class, we developed a pure functional programming
language, i.e., you could create and pass functions around or apply them to
other values. More importantly, all computations were expressed as function
applications---arguments went in and the computed value was returned. This is a
general model for expressing all kinds of computations, but it might still feel
limiting. Many of you are used to imperative languages (like C or Python), where
instead of function applications being nested, you are used to a sequence of
statements that execute with the program. The goal of this module is to learn
how to model such languages and understand how they work.

The key thing in these language is an @emph{implicit state}. As the language
executes individual statements, the statements compute a return value and/or
modify this implicit state. Contrast this with our functional language where the
only thing computed is the value returned, there is no state hidden away from
the programmer like what happens in imperative languages. Through the following
sections we will look at how we add state to our program.

We use @emph{state} and @emph{store} interchangeably.

@section{Syntax}

First, we extend the surface syntax to write programs that have sequences of
instructions. So we can write programs like below:

@#reader scribble/comment-reader
(examples #:eval ev
(eval '(seq
        (add1 45)
        (let ((x 42))
          (+ x 3))
        #t))
)

Here, the @tt{seq} block describes a sequence of expressions to be evaluated.
The result of the entire begin block is the result of the last statement, and
all intermediate results are discarded. However, if all intermediate results are
discarded there is no use of writing a sequence of statements! To access results
of previous statements in a program, one needs to add to the language the
ability to @emph{store} and @emph{retrieve} results of prior computations.

In its simplest form, storage is a place to put things so we can go back and get
them. Storage can come in many forms - tape, disk, memory, USB sticks, firmware
- but it always allows for values to persist in some form over time. Storage can
be viewed abstractly as a collection of locations that are values for storing
other values. Think of a location as a box and the value at a location as the
contents of the box. The location is a value in the sense that it is constant
and does not change, but the contents of a location can change. We dereference a
location when we want the value it stores. Remember, a location is a value and
as such can be calculated and returned without affecting the contained value.
Dereferencing gets the value out and returns it.

Storage exists in the background and is there to use whenever needed. When we
program we declare variables, pointers and references that all reference
storage, but we don’t ever talk about storage as a whole unless we’re systems
programmers. In C this means accessing the heap memory (using @tt{malloc} and
@tt{free}), or in languages like Python it just means the memory managed by the
language runtime.

To do access store, we add to the language 4 constructs: @tt{new}, @tt{deref},
@tt{set}, and @tt{free}. @tt{new} allocates a fresh location in the store. Think
of this as allocating some new memory and putting the result in that
location.@tt{deref} dereferences a given location to return the value stored in
that location. @tt{set} updates the location to store a new value, and @tt{free}
removes a location from the state thus freeing up space. Thus our previous
language is now extended to have the following forms:

@centered{@(scale (render-state-lang) 1.5)}

The state @(render-term L S) is a mapping of locations @(render-term L l) to
values @(render-term L v). The values in our language now also include
locations. These locations are abstract labels that just denotes a place where
we can store values. In some languages they are references, in languages like C
or C++, they are the direct memory addresses.

The term @racket[(seq e1 e2)] is simplified to show a sequence of two
operations. In practice, our language will support the more general form that
supports a sequence of @tt{n} expressions @racket[(seq e1 e2 ... en)].

Here is an example of a program using the state:

@#reader scribble/comment-reader
(examples #:eval ev
(eval '(let ((x (new 5)))
         (seq
          (set x (add1 (deref x)))
          (+ 3 (deref x)))))
)

This program creates a new location to store @racket[5] and binds @tt{x} to this
new location. Following that, it runs a sequence of statements, where it updates
the value stored in the location contained in @tt{x} to @racket[6]. Finally, it
adds @racket[3] to the updated value. This computation should return @racket[9]
as a result. Notice, as @tt{x} is a reference now, we have to use @tt{deref} to
dereference the value from the location stored in @tt{x}.

@section{Modeling the State}

We model state as a global store that maps locations to their values. This is a
very abstract model of state, but it is one of the most general ways to model
it. If you are concerned the environment sounds just like the state, then you
are right---they have a lot of similarities, but the biggest one is the
@emph{state is mutable}. Let us demonstrate this through 3 operations we define
on the state:

First, @tt{update} takes a state, a location, and a value and creates a new
state such that the provided location has the updated value. If such a location
does not exist, it it created in the state.

@centered{@(scale (render-update) 1.5)}

The @tt{get} function takes a state and a location and returns the value stored
in that location.

@centered{@(scale (render-get) 1.5)}

Finally, the @tt{del} function removes location from the state, freeing up
storage. Any further accesses to that location will now fail.

@centered{@(scale (render-del) 1.5)}

@section{Meaning of State}

Now we are ready to give precise meaning of programs that handle state. The
global state can change as the program is executing. In other words, as programs
were evaluated before they only produced a value, but now evaluation of programs
will require an initial state and once evaluated it will yield a final state and
the value produced from the computation.

To write this we will change the judgement of our formal rules. Now our judgment
will look like @tt{E ⊢ ⟨S, e⟩ ⇓ ⟨S, v⟩}. This means under a given environment
@tt{E}, our rules take a tuple of the initial state @tt{S} and expression @tt{e}
and evaluate it the final state @tt{S} and a value @tt{v}.

As an example, here is a selection of a few previous rules written to use the
state notation:

@centered{@(scale (render-eval-rules-judgment-old) 1.5)}

We add a state to the inference rules to show that it may change
whenever any expression is evaluated, and the evolution of the state now gives a
notion of ordering in our evaluation. For example, in add, the state starts off
with @tt{S}, but after evaluation of @tt{e1}, it may evolve to a different state
@tt{S1}. @tt{e2} is now evaluated under this state @tt{S1} to produce a new
state @tt{S2}, which is the final result of the evaluation of @racket[(+ e1
e2)]. Contrast this with previous rules of addition, it did not matter whether
we evaluated @tt{e1} or @tt{e2} before.

All other rules also incorporate a similar change for state. As an example, the
lambda and function application rule is also shown.

Now, we can focus on giving meaning to the newly added parts of the language:

@centered{@(scale (render-eval-rules-judgment) 1.5)}

The meaning of @tt{(seq e1 e2)} is to execute @tt{e1} and @tt{e2} in sequence.
The state evolves through the computation and is used for evaluating @tt{e2}.
The entire block evaluates to the meaning of @tt{e2}. The rule can be extended
for any number of expressions inside @tt{seq}.

The meaning of @tt{(new e)} is to allocate a new location in the store, and
store the meaning of @tt{e} in that location. The newly allocated location is
the meaning of @tt{(new e)}.

The meaning of @tt{(deref e)} is to dereference a location in the store, and get
the value stored at that location.

The meaning of @tt{(set e1 e2)} is to get the location to be updated from the
meaning of @tt{e1} and the value to be updated to from @tt{e2} and update the
location from @tt{e1} to the meaning of @tt{e2}.

The meaning of @tt{(free e)} is to delete the location from the meaning of
@tt{e} in the state and mean the deleted location.

Through all these operations the state might change because any subexpression
may modify the state. Hence, the state may evolve and it is threaded forward as
each of the subexpressions are evaluated.

@section{Interpreter for a Stateful Language}

If you notice carefully, unlike the environment which sends variables out of
scope when needed, the state does not do anything like that. The state behaves
like a global store, that has locations, once added persist until they are
removed. Thus, we can simplify our implementation by moving all the @tt{update},
@tt{get}, and @tt{del} operations by updating the state in-place. We will
represent the state in our implementation using a Racket hash table. The
function @racket[make-hash] creates an empty hash table.

For the @tt{seq} case, which just runs the @tt{interp} function on all the
sub-expressions one by one and the final value is returned. The state @tt{S} is
updated in-place so as long as the evaluation of the subexpression happens in
sequence it matches the meaning of @tt{(seq e1 e2 ... e3)}.

@#reader scribble/comment-reader
(examples #:eval ev
(define (interp-seq E S es)
  (match es
    [(cons e '()) (interp E S e)]
    [(cons e es)  (begin
                    (interp E S e)
                    (interp-seq E S es))]))
)

For the @tt{new} case, we define @tt{interp-new}, we first need a fresh
location. We use the Racket provided @racket[gensym] function for this. It is
guaranteed to produce a fresh symbol each time it is called. The subexpression
@tt{e} is evaluated and it is stored in the hash (i.e., our state) with @tt{l}
mapping to the value and the location is returned.

@#reader scribble/comment-reader
(examples #:eval ev
(define (interp-new E S e)
  (let ((v (interp E S e))
        (l (gensym)))
    (begin
      (hash-set! S l v)
      l)))
)

To handle @tt{deref}, we define @tt{interp-deref}, where our interpreter
evaluates the subexpression and looks up the hash for that location which is
then returned.

@#reader scribble/comment-reader
(examples #:eval ev
(define (interp-deref E S e)
  (let ((l (interp E S e)))
    (hash-ref S l)))
)

For handling @tt{set}, the two subexpressions are evaluated and then the
location is updated with the new value from the second subexpression.

@#reader scribble/comment-reader
(examples #:eval ev
(define (interp-set E S e1 e2)
  (let ((l (interp E S e1))
        (v (interp E S e2)))
    (begin
      (hash-set! S l v)
      v)))
)

Finally, the @tt{free} function removes the given reference from the state. Any
future access to the same location will result in an error.

@#reader scribble/comment-reader
(examples #:eval ev
(define (interp-free E S e)
  (let ((l (interp E S e)))
    (begin0
      (hash-remove! S l)
      l)))
)

Putting all these pieces together, the full interpreter looks like below: 

@codeblock-include["state/interp.rkt"]

Running the interpreter now requires an empty environment and an empty state, so
we can write a convenience function, @tt{eval}, that instantiates these and calls
the @tt{interp} function.

@#reader scribble/comment-reader
(examples #:eval ev
(define (eval e)
  (let ((E '())
        (S (make-hash)))
    (interp E S e)))
)

@section{Testing}

We can write a few tests to check if the interpreter is behaving as we would
expect:

@#reader scribble/comment-reader
(examples #:eval ev
(eval '(let ((x (new 5)))
         (seq
          (set x (add1 (deref x)))
          (+ 3 (deref x)))))
)

Previously, in @secref{Lambda}, we used the Y-combinator with call-by-name to
write recursive functions, but now we can do it more naturally.

@#reader scribble/comment-reader
(examples #:eval ev
(eval '(let ((fact (new #f)))
         (seq
          (set fact (λ (n)
                      (if (zero? n) 1
                          (* n ((deref fact) (sub1 n))))))
          ((deref fact) 5))))
)

These can be written down as test cases:

@#reader scribble/comment-reader
(examples #:eval ev
(check-eqv? (eval '(let ((x (new 5)))
                     (seq
                      (set x (add1 (deref x)))
                      (+ 3 (deref x))))) 9)

(check-eqv? (eval '(let ((fact (new #f)))
                     (seq
                      (set fact (λ (n)
                                  (if (zero? n) 1
                                      (* n ((deref fact) (sub1 n))))))
                      ((deref fact) 5)))) 120)
)

@section{Aliasing and Mutability}

@bold{TODO}
