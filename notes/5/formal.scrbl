#lang scribble/manual

@(require "../fancyverb.rkt" "../utils.rkt")
@(require redex pict "model.rkt" "truth-table.rkt")
@(require (for-label racket rackunit))
@(require scribble/examples racket/sandbox scribble-math)

@(define codeblock-include (make-codeblock-include #'here))

@(ev '(require rackunit))
@(for-each (λ (f) (ev `(require (file ,(path->string (build-path "examples" "arithmetic" f))))))
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
   (parameterize ([current-directory (build-path "examples" "arithmetic")])
     (filebox (emph "shell")
              (fancyverbatim "fish" (apply shell s)))))

@title[#:tag "Formal" #:style (with-html5 manual-doc-style)]{Formal Semantics}

One way to think about the world of languages (or the world in general) is in terms of @emph{formal systems}. Attributed to David Hilbert and Gotlieb Frege, a formal system provides mechanisms for representing and reasoning a out systems. The term @emph{formal} implies principled or formal in a mathematical sense.

@table-of-contents[]

@section{Syntax}

The syntax of a formal system defines the form of terms in that system. Syntax is frequently defined using a @emph{grammar} as we used in the preceeding notes. We’re not going to do much with syntax, so little needs to be said other than providing a basic definition.

The @emph{alphabet} of a grammar is a set of atomic symbols representing syntax elements that cannot be decomposed further. The @emph{rules} of a grammar define legal orderings of symbols. The set of strings that are in the closure of the alphabet with respect to application of grammar rules is defined as the formal language described by the grammar.

As an example, consider the grammar of a subset of propositional logic:

@centered{@(scale (renderer (λ () (render-language L))) 1.5)}

This format should be similar to the previous notes. The alphabet includes terminal symbols including @tt{true} and @tt{false}, but also symbols such as @tt{∧}, @tt{∨}, @tt{¬}. The @emph{variable} is a shorthand for all identifiers representing propositions. Grammar rules define @tt{⇒}, @tt{⇔}, @tt{∧}, and @tt{∨} as binary operations and @tt{¬} as a unary operator. The recursive nature of grammar rules over @tt{t} allows arbitrary nesting of terms.

@section{Inference Systems}

@emph{Inference systems} are defined by @emph{axioms} and @emph{inference rules}. They allow derivation of true statements from other true statements. You have done this whenever you use mathematics to simplify a formula or solve equations for a quantity.

We all know, for example, that @${(x + y)^2 = x^2 + 2xy + y^2}. We also know that @${(x + y)^2 = x^2 + 2xy + y^2} is true in algebra regardless of what @${x} and @${y} represent. We could define this relationship using inference rules:

@$${\frac{(x + y)^2}{(x^2 + 2xy + y^2)}}

@centered{or an equivalence}

@$${\frac{(x^2 + 2xy + y^2)}{(x + y)^2}}

The classical notation for inference rules was defined in the previous chapter. The inference rule:

@$${\frac{P_0 \quad P_1 \quad \ldots P_n}{C}}

states that when @${P_0} through @${P_n} are true, then @${C} is also true. The @${P}’s are often referred to as @emph{premise} or @emph{antecedents} while @${C} is the @emph{conclusion} or @emph{consequent}. The set of premise may be arbitrarily large, but there is only one conclusion associated with a rule. The special case when the set of premise is empty:

@centered{@(scale (renderer (λ () (derivation->pict L (derivation 'P "axiom" '())))) 1.5)}

defines an @emph{axiom}. Nothing need be true for @tt{P} to be true, therefore it is always true.

As an example inference system we’ll look at @emph{propositional logic}, the logic of true and false propositions that defines the heart of classical logic. We’ll start with one axiom that @tt{true} is always true:

@centered{@(scale (renderer (λ () (derivation->pict L (derivation 'true "true" '())))) 1.5)}

Nothing need be known to assert @tt{true} is trivially true. It turns out that @tt{true} doesn’t tell us much, but it does serve as a value in our logic. The other value is @tt{false}. Consider what axioms or inference rules might have @tt{false} as a consequent. Are there any?

Other inference rules define introduction and elimination rules for various operators. Introduction rules introduce their associated operation in an expression. The introduction rule for @(renderer (λ () (render-term L (and X Y)))) is:

@centered{@(scale (renderer (λ () (derivation->pict L (derivation '(and X Y) "and" (list (derivation 'X "given" '())
                                                                                         (derivation 'Y "given" '())))))) 1.5)}

If @tt{X} and @tt{Y} are both known, then @(renderer (λ () (render-term L (and X Y)))) is immediately true.

Elimination rules are the inverse of introduction rules. There are two for @(renderer (λ () (render-term L (and X Y)))):

@centered{@(scale (renderer (λ () (derivation->pict L (derivation 'X "and-elim-1" (list (derivation '(and X Y) "given" '())))))) 1.5)}
@centered{@(scale (renderer (λ () (derivation->pict L (derivation 'Y "and-elim-2" (list (derivation '(and X Y) "given" '())))))) 1.5)}

Each rule allows one conjunct to be inferred from the conjunction. The first giving the left conjunct and the second the right. Note that introduction rules make larger terms from smaller term while elimination rules make smaller terms from larger terms. This will have important consequences when we talk about proofs.

Speaking of proofs, we now have a tiny subset of the inference rules defining propositional logic. How do we use them? Let’s do a quick derivation that combines inference rules.
Specifically, let’s prove the commutative property of conjunction, @(renderer (λ () (render-term L (T (and A B) (and B A))))). We start by assuming @(renderer (λ () (render-term L (and A B)))) and using derivation rules to make inferences:

@centered{@(scale (renderer (λ () (derivation->pict L (derivation '(and B A) "and" (list (derivation 'A "and-elim-1" (list (derivation '(and A B) "given" '())))
                                                                                         (derivation 'B "and-elim-2" (list (derivation '(and A B) "given" '())))))))) 1.5)}

Note how the inference rules click together like Legos. Conclusion of rules plug into the premise of others. With a derivation from premise to conclusion we can say @(renderer (λ () (render-term L (T (and A B) (and B A))))).
The @(renderer (λ () (render-term L (T X Y)))) operator indicates there is a derivation from @tt{X} to @tt{Y} and we can skip the details in other derivations. If @tt{X} is empty, we say that @(renderer (λ () (render-term L (T true Y)))) is a theorem. Because it assumes nothing to start with, a theorem can be used anywhere.

We can add other inference rules for remaining logical operations. The elimination rule for @tt{¬} is the double negative rule from classical logic:

@centered{@(scale (renderer (λ () (derivation->pict L (derivation 'X "not" (list (derivation '(not (not X)) "given" '())))))) 1.5)}

The introduction rule for @tt{¬} is more interesting as a derivation is one of the premise. The premise of the elimination rule says that assuming @tt{X} gives @tt{Y} and @(renderer (λ () (render-term L (not Y)))) is also known. This is a contradiction because @tt{X} and @(renderer (λ () (render-term L (not X)))) cannot be simultaneously true. Thus, @tt{X} must be false:

@centered{@(scale (renderer (λ () (derivation->pict L (derivation '(not X)  "not" (list (derivation '(T X Y) "given" '())
                                                                                        (derivation '(not Y) "given" '())))))) 1.5)}

This is the classic proof by contradiction. It also suggests that if @tt{false} is ever true, we have big problems because we can derive anything.

The rules for implication again perform eliminate and introduction of @(renderer (λ () (render-term L (==> X Y)))). The elimination rule is known as @emph{modus ponens} and says that if @tt{X} and @(renderer (λ () (render-term L (==> X Y)))) are known, then @tt{Y} is also known:

@centered{@(scale (renderer (λ () (derivation->pict L (derivation 'Y "implies-elim" (list (derivation 'X "given" '())
                                                                                          (derivation '(==> X Y) "given" '())))))) 1.5)}

The introduction rule has a derivation in the premise. It says that if we can derived @tt{Y} from @tt{X}, then @tt{X} implies @tt{Y} or @(renderer (λ () (render-term L (==> X Y)))):

@centered{@(scale (renderer (λ () (derivation->pict L (derivation '(==> X Y) "implies-intro" (list (derivation '(T X Y) "given" '())))))) 1.5)}

If assuming @tt{X} allows us to derive @tt{Y}, then we also know that @(renderer (λ () (render-term L (==> X Y)))).

Finally, we have introduction and elimination rules for logical equivalence.

@centered{@(scale (renderer (λ () (derivation->pict L (derivation '(<=> (and A B) (and B A)) "equiv" (list (derivation '(==> (and A B) (and B A)) "implies-intro" (list (derivation '(T (and A B) (and B A)) "given" '())))
                                                                                                           (derivation '(==> (and B A) (and A B)) "implies-intro" (list (derivation '(T (and B A) (and A B)) "given" '())))))))) 1.5)}

There is much more we can do with inference rules and systems, but this brief demonstration should give you an idea of how these things define formal reasoning processes. Just like Lego, simple things fit together in simple ways to develop complex and elegant systems.

@section{Semantics}

A language’s semantics gives its structures meaning. When we used inference rules to define how we reason about propositional logic, we provided a reasoning mechanism without regard to meaning. We could have changed the inference rules in a very simple way and gotten something that is not at all propositional logic. Let’s say we defined a completely wrong rule for implication like this:

@centered{@(scale (renderer (λ () (derivation->pict L (derivation 'X "wrong" (list (derivation 'Y "given" '())
                                                                                   (derivation '(==> X Y) "given" '())))))) 1.5)}

Clearly this is not how implication works, but it is a perfectly fine rule and we can reach conclusions from it. What makes it incorrect is the semantics of propositional logic. Semantics defines the meanings of language expressions using another mathematical system.

For propositional logic we can use the common notion of a truth table to define our operations:



@tabular[#:style 'boxed
         #:sep @hspace[1]
         #:row-properties '(bottom-border)
         (cons (list @bold{X}
                     @bold{Y}
                     @bold{@(renderer (λ () (render-term L (and X Y))))})
               @(bool-ops (λ (x y) (and x y))))]
               
@tabular[#:style 'boxed
         #:sep @hspace[1]
         #:row-properties '(bottom-border)
         (cons (list @bold{X}
                     @bold{Y}
                     @bold{@(renderer (λ () (render-term L (or X Y))))})
               @(bool-ops (λ (x y) (or x y))))]

@tabular[#:style 'boxed
         #:sep @hspace[1]
         #:row-properties '(bottom-border)
         (cons (list @bold{X}
                     @bold{Y}
                     @bold{@(renderer (λ () (render-term L (==> X Y))))})
               @(bool-ops (λ (x y) (if x y #t))))]

@tabular[#:style 'boxed
         #:sep @hspace[1]
         #:row-properties '(bottom-border)
         (cons (list @bold{X}
                     @bold{Y}
                     @bold{@(renderer (λ () (render-term L (<=> X Y))))})
               @(bool-ops (λ (x y) (and (if x y #t) (if y x #t)))))]

@tabular[#:style 'boxed
         #:sep @hspace[1]
         #:row-properties '(bottom-border)
         (cons (list @bold{X}
                     @bold{@(renderer (λ () (render-term L (not X))))})
               '(("T" "F")
                 ("F" "T")))]

We can't easily derive new truths using simple truth tables, but we can with the inference system. To ensure the inference system only produces correct results we can compare it with what is specified in the truth tables. Let’s look at our broken rule for negation:

@centered{@(scale (renderer (λ () (derivation->pict L (derivation 'X "wrong" (list (derivation 'Y "given" '())
                                                                                   (derivation '(==> X Y) "given" '())))))) 1.5)}

The rule says that if @tt{X} and @tt{Y} are both true, then @tt{X} must also be true. Looking at the truth table for @tt{⇒} clearly says otherwise. When @tt{Y} is true and @(renderer (λ () (render-term L (==> X Y)))) is true in the second row, @tt{X} is false.

@section{Discussion}

Thinking of languages and mathematical systems as formal systems will serve you well. Throughout this class we will think of languages in terms of what they look like (syntax), how to manipulate them (inference system), and what they mean (semantics). At times the Hilbert system structure will be difficult to see, but it is always there.
