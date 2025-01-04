#lang scribble/manual

@(require "../fancyverb.rkt" "../utils.rkt")
@(require scribble/core racket/list)
@(require (for-label racket rackunit))
@(require scribble/examples racket/sandbox)

@(define codeblock-include (make-codeblock-include #'here))

@(define core-racket
  (parameterize ([sandbox-output 'string]
                 [sandbox-error-output 'string]
                 [sandbox-memory-limit 50])
    (make-evaluator 'racket)))

@(core-racket '(require racket/match))

@(define-syntax-rule (ex e ...)
  (filebox (emph "Racket REPL")
    (examples #:eval core-racket #:label #f e ...)))

@(define (hs-repl . s)
  (filebox (emph "GHCi REPL")
    (apply fancyverbatim "haskell" s)))

@(define (shellbox . s)
   (parameterize ([current-directory (build-path "examples")])
     (filebox (emph "shell")
              (fancyverbatim "fish" (apply shell s)))))

@title[#:tag "Hs2Rkt"]{From Haskell to Racket}

@table-of-contents[]

@section{Basic Values}

Let’s start by looking at something you know: Haskell. In Haskell, expressions can include literals for numbers, strings, booleans. Here we are using the Haskell's GHCi which provides a read-eval-print-loop (REPL) to type in examples and evaluate their results:

@hs-repl{
> 8
8
> "haskell"
"haskell"
> True
True
> False
False
}

Note, evaluating values in GHCi gives the value back. Despite Haskell being a typed language, GHCi does not print the types of the expressions by default. We can use @tt{:set +t} to require GHCi to print the type of the expression it evaluates.

@hs-repl{
> :set +t -- tell GHCi to automatically show types
> 8
8
it :: Num a => a
> "haskell"
"haskell"
it :: [Char]
> True
True
it :: Bool
> False
False
it :: Bool
}

GHCi now prints the type of the expression it evaluated as @tt{it :: Type}. Here @tt{it} refers to the last evaluated expression. Note how @tt{8} is not printed as an @tt{Integer} but as @tt{Num a => a}. It means @tt{8} has a type @tt{a} where @tt{a} is an instance of the @tt{Num} typeclass. The @tt{Num} typeclass describes common properties of various kinds of number, such as for example. The native machine integer type @tt{Int}, arbitrary-sized integers @tt{Integer}, and even floating point type @tt{Double} are instances of @tt{Num}.

The Racket REPL also operates similarly:

@ex[
8
"racket"
#t
#f
]

Racket only prints the value as it is an untyped language. The notation for booleans is slightly different, but both languages agree on numbers, strings, and booleans. The languages are essentially the same so far.

@section{Basic Operations}

Haskell uses an infix notation for writing operations.

@hs-repl{
> 1 + 2
3
it :: Num a => a
}

The order of operations follows the usual mathematical precedence rules (which you must memorize), or you can use parentheses to indicate grouping:

@hs-repl{
> 1 + (2 * 2)
5
it :: Num a => a
> (1 + 2) * 2
6
it :: Num a => a
}

Extraneous parenthesis are fine:

@hs-repl{
> (((1))) + ((2 * 2))
5
it :: Num a => a
}

Compared to many languages you may know, including Haskell, Racket employs a uniform, minimalistic concrete syntax based on the concept of parenthesized, prefix notation.

In this notation, parentheses play a much more central role. They are not optional and they signal the form of the expression.

Languages, like people, descend from their ancestors and inherit some of their properties. In the case of notation, Racket inherits the Lisp (and Scheme) notation for programs. It takes a bit of getting used to, but once acclimated, the notation should feel lightweight and consistent; there is very little to memorize when it comes to syntax.

So in Racket, we would write:

@ex[
(+ 1 (* 2 2))
(* (+ 1 2) 2)
]

Note that there are no precedence rules for addition and multiplication: the form of the expression makes it unambiguous.

Parenthesis indicate function applications, so adding extraneous parens means something different than in Haskell:

@ex[
(eval:error (1)) ]

Here the parens are indicating a function application.  The ``function'' is the first subexpression within the parens, i.e. @racket[1].  Of course, @racket[1] isn't a function and can't be applied, hence the error.

@section{Functions}

Haskell also has a notation for writing functions:

@hs-repl{
> \x y -> x + y
<interactive>:18:1: error:
    * No instance for (Show (Integer -> Integer -> Integer))
        arising from a use of 'print'
        (maybe you haven't applied a function to enough arguments?)
    * In a stmt of an interactive GHCi command: print it
}

The REPL prints the values of the expression it evaluated. Once functions are evaluated, their source cannot be printed. However, we can still ask GHCi to print the type of the expression by preceding it with a @tt{:t}.

@hs-repl{
> :t \x y -> x + y
(\x y -> x + y) :: Num a => a -> a -> a
}

This make an anonymous function that consumes two numbers and produces their sum.

To apply it, we can write it juxtaposed with arguments:

@hs-repl{
> (\x y -> x + y) 3 4
7
it :: Num a => a
}

Note that in Haskell, every function is a function of exactly one argument. Therefore @tt{\x y -> x + y} is actually shorthand for @tt{\x -> \y -> x + y}.

Applying such a function to fewer than 2 arguments will do a partial function application, which will produce a function that take the remaining arguments:

@hs-repl{
> :t (\x y -> x + y) 3
(\x y -> x + y) 3 :: Num a => a -> a
}

To encode functions that must always be given two arguments, a tuple can be used:

@hs-repl{
> :t \(x, y) -> x + y
\(x, y) -> x + y :: Num a => (a, a) -> a
}

To apply such a function, it must be given a pair of integers:

@hs-repl{
> (\(x, y) -> x + y)(3, 4)
7
it :: Num a => a
}

The use of (x, y) here in the function parameters is actually a pattern. This can be understood as shorthand for:

@hs-repl{
> :t \p -> case p of (x, y) -> x + y
\p -> case p of (x, y) -> x + y :: Num a => (a, a) -> a
}

So even this function is actually taking a single argument (which must be a pair of numbers).

Racket has a similar notation for writing functions:

@ex[
(λ (x) (λ (y) (+ x y)))
]

You can also write this without the fancy @racket[λ] by spelling it @racket[lambda]:

@ex[
(lambda (x) (lambda (y) (+ x y)))
]

(In DrRacket, to insert a @racket[λ] press Cmd + \)

To apply it, it must be written in parens, juxtaposed with arguments:

@ex[
(((λ (x) (λ (y) (+ x y))) 3) 4)
]

Functions in Racket do not always consume a single argument. They can consume 0, 1, or more arguments.

@ex[
(λ (x y) (+ x y))
]

This is not a shorthand for the function above it; rather it is a function that expects two arguments:

@ex[
((λ (x y) (+ x y)) 3 4)
]

Applying a function to the wrong number of arguments will result in an error (and not perform partial function application):

@ex[
(eval:error ((λ (x y) (+ x y)) 3))
]

@section{Definitions}

In Haskell, variables can be defined with @tt{let}:

@hs-repl{
> let x = 3
x :: Num a => a
> let y = 4
y :: Num a => a
> x + y
7
it :: Num a => a
> :{
  let fact = \n -> case n of
              0 -> 1
              n -> n * (fact (n - 1))
  :}
fact :: (Eq a, Num a) => a -> a
> fact 5
120
it :: (Eq a, Num a) => a
}

The @tt{@":{"} and @tt{@":}"} marks the start and end of a multi-line definition in the REPL.

In Racket, variables are defined with the @racket[define] form:

@ex[
(define x 3)
(define y 4)
(+ x y)
(define fact
  (λ (n)
    (match n
      [0 1]
      [n (* n (fact (- n 1)))])))
(fact 5)
]

In Haskell, function definitions can be written as:

@hs-repl{
let fact n = case n of
                  0 -> 1
                  n -> n * (fact (n - 1))
}

This is just a shorthand for the definition written above in terms of @tt{\}.

Similarly in Racket, function definitions can be written as:

@ex[
(define (fact n)
  (match n
    [0 1]
    [n (* n (fact (- n 1)))]))
]

which is shorthand for the definition above using @racket[λ].

Notice both Haskell and Racket have pattern matching forms, which are quite useful for writing function in terms of a number of ``cases.'' More on this in a minute.

@section{Lists}

Haskell has a built-in list datatype. The empty list is written @tt{[]} and @tt{:} is an operation for ``consing'' an element on to a list. So to build a list with three integer elements, 1, 2, and 3, you'd write:

@hs-repl{
> 1 : 2 : 3 : []
[1,2,3]
it :: Num a => [a]
}

Racket has a built-in list datatype. The empty list is written @racket['()] and @racket[cons] is an operation for consing an element on to a list. To build the same list, you'd write:

@ex[
(cons 1 (cons 2 (cons 3 '())))
]

The notation @racket[(list 1 2 3)] is shorthand for the above.

There is a slight difference here. For one, Haskell lists must be homogeneous. You can have a list of strings or a list of numbers, but you can't have a list of strings and numbers.

@hs-repl{
> ["a", 3]
<interactive>:45:7: error:
    * No instance for (Num [Char]) arising from the literal '3'
    * In the expression: 3
      In the expression: ["a", 3]
      In an equation for 'it': it = ["a", 3]
}

In Racket, there is no such restriction:

@ex[
(list "a" 3)
]

Also, in Racket, @racket[cons] plays the role of both tupling (making pairs) and making lists (making a pair of an element and another list).

So in Haskell, you could make a pair @tt{("a", 3)}. In Racket, you'd write @racket[(cons "a" 3)]. Note this is a pair and not a proper list. In Haskell, tuples and lists are disjoint things. In Racket, lists and tuples (pairs) are made out of the same stuff.

This can be confusing the first time you encounter it, so let's go over it a bit more.

In Racket (or any Lisp), @racket[cons] plays the role of both the pair constructor and the list constructor. Non-empty lists are a subset of pairs: they are pairs whose second component is a list (either the empty list or another pair whose second component is a list, etc.).

You can make pairs out of any kind of element and you can make lists out of any kind of elements. We can precisely define these sets as:

@#reader scribble/comment-reader
(ex
;; type ListofAny =
;; | '()
;; | (cons Any ListofAny)

;; type PairofAny =
;; | (cons Any Any)
)

Or, to give more useful parameterized definitions:

@#reader scribble/comment-reader
(ex
;; type (Listof A) =
;; | '()
;; | (cons A (Listof A))

;; type (Pairof A B) =
;; | (cons A B)
)

The functions @racket[first] and @racket[rest] operate on non-empty lists, producing the first element of the list and the tail of the list, respectively.

@ex[
(first (cons 3 (cons 4 '())))
(rest (cons 3 (cons 4 '())))
]

These function will produce errors if given something that is a pair but not a list:

@ex[
(eval:error (first (cons 3 4)))
(eval:error (rest (cons 3 4)))
]

On the other hand, the functions @racket[car] and @racket[cdr] access the left and right components of a pair (the names are admittedly awful and an artifact of Lisp history):

@ex[
(car (cons 3 4))
(cdr (cons 3 4))
]

When given pairs that are also lists, they behave just like @racket[first] and @racket[rest]:

@ex[
(car (cons 3 (cons 4 '())))
(cdr (cons 3 (cons 4 '())))
]

@section{Pattern Matching}

Haskell has a very nice pattern matching for letting you express case analysis and decomposition in a concise way.

Each pattern matching expression has a sub-expression that produce a value to be matched against and a number of clauses. Each clause has a pattern and an expression. The pattern potentially consists of data constructors, variables, and literals. If the value matches the first pattern, meaning the value and the template match up on constructors and literals, then the variables are bound to the correspond parts of the value, and the right-hand side expression is evaluated. If the value doesn't match, the next pattern is tried, and so on. It's an error if none of the patterns match.

So for example, we can write a function that recognize even digits as:

@hs-repl{
> :{
  let evenDigit n = case n of 0 -> True
                              2 -> True
                              4 -> True
                              6 -> True
                              8 -> True
                              _ -> False
  :}
evenDigit :: (Eq a, Num a) => a -> Bool
}

The patterns here, save the last one, are just integer literals. If @tt{n} is the same as any of these integers, the value @tt{True} is produced. The last case uses a ``wildcard,'' which matches anything and produces @tt{False}.

Here's an example that matches a tuple, binding each part of the tuple to a name and then using those names to construct a different tuple:

@hs-repl{
> let swap p = case p of (x, y) -> (y, x)
swap :: (b, a) -> (a, b)
}

Here the pattern uses a data constructor (the tuple constructor). It matches any value that is made with the same constructor.

Here is a recursive function for computing the sum of a list of numbers, defined with pattern matching:

@hs-repl{
> :{
let addNums xs = case xs of []   -> 0
                            x:xs -> x + (addNums xs)
:}
addNums :: Num p => [p] -> p
> addNums [4,5,6]
15
it :: Num p => p
}

We can do the same in Racket:

@ex[
(define (even-digit n)
  (match n
    [0 #t]
    [2 #t]
    [4 #t]
    [6 #t]
    [8 #t]
    [_ #f]))
(define (swap p)
  (match p
    [(cons x y) (cons y x)]))
(define (sum xs)
  (match xs
    ['() 0]
    [(cons x xs)
      (+ x (sum xs))]))
(sum (list 4 5 6))
]

@section{Datatypes}

Haskell has the ability to declare new datatypes. For example, we can define type for binary trees of numbers:

@hs-repl{
data BinaryTree = Leaf
                | Node Integer BinaryTree BinaryTree
}

This declares a new type, named @tt{BinaryTree}. There are two variants of the @tt{BinaryTree} type, each with their own constructor: @tt{Leaf} and @tt{Node}. The @tt{Leaf} constructor takes no arguments, so just writing @tt{Leaf} creates a (empty) binary tree:

@hs-repl{
> :t Leaf
Leaf :: BinaryTree
}

The @tt{Node} constructor takes three arguments: an integer and two binary trees. Applying the constructor to a tuple of three things, makes a (non-empty) binary tree:

@hs-repl{
> :t Node 3 Leaf Leaf
Node 3 Leaf Leaf :: BinaryTree
}

Binary trees are an example of a recursive datatype, since one of the variants contains binary trees. This means we can build up arbitrarily large binary trees by nesting nodes within nodes:

@hs-repl{
> :t Node 3 (Node 4 Leaf Leaf) (Node 7 Leaf Leaf)
Node 3 (Node 4 Leaf Leaf) (Node 7 Leaf Leaf) :: BinaryTree
}

Pattern matching is used to do case analysis and deconstruct values. So for example, a function that determines if a binary tree is empty can be written as:

@hs-repl{
> :{
let btEmpty bt = case bt of Leaf       -> True
                            Node _ _ _ -> False
:}
btEmpty :: BinaryTree -> Bool
> btEmpty Leaf
True
it :: Bool
> btEmpty (Node 4 Leaf Leaf)
False
it :: Bool
}

The patterns use the constructor names to discriminate on which constructor was used for a given binary tree. The use of the wildcard here is just saying it doesn't matter what's inside a node; if you're a node, you're not empty.

Recursive functions work similarly, but use variables inside patterns to bind names to the binary trees contained inside a node:

@hs-repl{
> :{
let btHeight bt = case bt of Leaf       -> 0
                             Node _ l r -> 1 + (max (btHeight l) (btHeight r))
:}
btHeight :: (Num p, Ord p) => BinaryTree -> p
> btHeight Leaf
0
it :: (Num p, Ord p) => p
> btHeight (Node 4 (Node 2 Leaf Leaf) Leaf)
2
it :: (Num p, Ord p) => p
}

We do something very similar in Racket using structures. A structure type is like a (single) variant of a data type in Haskell: it's a way of combining several things into one new kind of value.

@ex[
(struct leaf ())
(struct node (i left right))
]

This declares two new kinds of values: leaf structures and node structures. For each, we get a constructor, which is a function named after the structure type. The `leaf` constructor takes no arguments. The node constructor takes 3 arguments.

@ex[
(leaf)
(node 5 (leaf) (leaf))
(node 3 (node 2 (leaf) (leaf)) (leaf))
]

There is no type system in Racket, but we can conceptually still define what we mean in a comment. Just like in Haskell, we can use pattern matching to discriminate and deconstruct:

@ex[
(code:comment "type BinaryTree = (leaf | (node Integer BinaryTree BinaryTree))")
(define (bt-empty? bt)
  (match bt
    [(leaf) #t]
    [(node _ _ _) #f]))
(bt-empty? (leaf))
(bt-empty? (node 5 (leaf) (leaf)))
(define (bt-height bt)
  (match bt
    [(leaf) 0]
    [(node _ left right)
      (+ 1 (max (bt-height left)
                (bt-height right)))]))
(bt-height (leaf))
(bt-height (node 4 (node 2 (leaf) (leaf)) (leaf)))
]

@section{Symbols}

One of the built-in datatypes we will use often in Racket is that of a symbol. A symbol is just an atomic piece of data. A symbol is written using the @racket[quote] notation @racket[(quote symbol-name)], which is abbreviated @racket['symbol-name]. What's allowable as a symbol name follows the same rules as what's allowable as a Racket identifier.

Symbols don't have a whole lot of operations. The main thing you do with symbols is tell them apart from each other:

@ex[
(equal? 'fred 'fred)
(equal? 'fred 'wilma)
]

It is possible to convert between symbols and strings:

@ex[
(symbol->string 'fred)
(string->symbol "fred")
]

There's also a convenient function that produces a symbol that is guaranteed to have not been used so far each time you call it:

@ex[
(gensym)
(gensym)
(gensym)
]

They can be used to define ``enum'' like datatypes:

@ex[
(code:comment "type Flintstone = 'fred | 'wilma | 'pebbles")
]

You can use pattern matching to match symbols:

@ex[
(define (flintstone? x)
  (match x
    ['fred #t]
    ['wilma #t]
    ['pebbles #t]
    [_ #f]))
(flintstone? 'fred)
(flintstone? 'barney)
]

There's really not a precise analog to symbols in Haskell.

@section{Quote, quasiquote, and unquote}

One of the distinguishing features of languages in the Lisp family
(such as Scheme and Racket) is the @racket[quote] form.

The ``tick'' character @racket['d] is used as a shorthand for
@racket[(code:quote d)].

You've already seen it show up with symbols: @racket['x] is the symbol
@tt{x}.  It also shows up in the notation for the empty list:
@racket['()].

But you can also write @racket[quote] around non-empty lists like
@racket['(x y z)].  This makes a list of symbols.  It is equivalent to
saying @racket[(list 'x 'y 'z)].

In fact, you can nest lists within the quoted list: @racket['((x) y (q
r))].  This is equivalent to @racket[(list (list 'x) 'y (list 'q 'r))].

Here's another: @racket['(() (()) ((())))].  This is equivalent to
@centered{
@racket[(list '() (list '()) (list (list '())))]
}

So, anything you can write with quoted lists, you can write without
quoted lists by pushing the quote inward until reaching a symbol or an
empty set of parenthesis.

You can also put strings, booleans, and numbers inside of a
@racket[quote].  As you push the quote inward, it simply disappears
when reaching a string, boolean or number.  So @racket['5] is just
@racket[5].  Likewise @racket['#t] is @racket[#t] and @racket['"Fred"]
is @racket["Fred"].

You can also write pairs with @racket[quote], which uses the @tt{.}
notation for separating the left and right part of the pair.  For
example, @racket['(1 . 2)] is equivalent to @racket[(cons 1 2)].  If
you write something like @racket['(1 2 3 . 4)], what you are in effect
saying is @racket[(cons 1 (cons 2 (cons 3 4)))], an improper list that
ends in @racket[4].

In essence, @racket[quote] is a shorthand for conveniently
constructing data and is a very concise notation for writing down
ad-hoc data.  It serves much the same purpose as formats like JSON and
XML, except there's even less noise.

To summarize, with @racket[quote], you can construct

@itemlist[
@item{strings}
@item{booleans}
@item{numbers}
@item{symbols}
@item{and... pairs (or lists) of those things (including this one)}
]

The kind of things you can construct with the @racket[quote] form are
often called @bold{s-expressions}, short for @bold{symbolic
expressions}.

We can give a type definition for s-expressions:

@#reader scribble/comment-reader
(ex
;; type S-Expr =
;; | String
;; | Boolean
;; | Number
;; | Symbol
;; | (Listof S-Expr)
)

The reason for this name is because anything you can write
down as an expression, you can write down inside a
@racket[quote] to obtain @emph{a data representation} of
that expression. You can render an expression as a symbolic
representation of itself.

For example, @racket[(+ 1 2)] is an expression.  When run, it applies
the @emph{function} bound to the variable @racket[+] to the arguments
@racket[1] and @racket[2] and produces @racket[3].  On the other hand:
@racket['(+ 1 2)] constructs a peice of data, namely, a list of three
elements.  The first element is the @emph{symbol} @tt{+}, the second
element is @racket[2], the third element is @racket[3].


We will be using (subsets of) s-expressions extensively as our data
representation of AST and IR expressions, so it's important to gain a
level of fluency with them now.

Once you understand @racket[quote], moving on to @racket[quasiquote],
@racket[unquote], and @racket[unquote-splicing] are pretty
straight-forward.

Let's start with @racket[quasiquote]. The ``backtick''
character @racket[`d] is used as a shorthand for @tt{
 (quasiquote d)} and the ``comma'' character @racket[,e] is
shorthand for @tt{(unquote e)}. The @tt{(quasiquote d)} form
means the same thing as @tt{(quote d)}, with the exception
that if @tt{(unquote e)} appears anywhere inside @tt{d},
then the @emph{expression} @racket[e] is evaluated and it's
value will be used in place of @tt{(unquote e)}.

This gives us the ability to ``escape'' out of a quoted
peice of data and go back to expression mode.

If we think of @racket[quasiquote] like @racket[quote] in
terms of ``pushing in'' then the rules are exactly the same
except that when a @racket[quasiquote] is pushed up next to
an @racket[unquote], the two ``cancel out.'' So @racket[`,e] is
just @tt{e}.  

For example, @racket[`(+ 1 ,(+ 1 1))] is equivalent to
@racket[(list '+ 1 (+ 1 1))], which is equivalent to
@racket[(list '+ 1 2)].

So if @racket[quote] signals us to stop interpreting things
as expressions, but instead as data, @racket[quasiquote]
signals us to stop interpreting things as expression, but
instead as data.. @emph{unless we encounter a
 @racket[unquote]}, in which case you go back to interpreting
things as expressions.


The last remaining peice is @racket[unquote-splicing], which
is abbreviated with ``comma-at'': @racket[,@e] means @tt{
 (unquote-splicing e)}. The @racket[unquote-splicing] form is
like @racket[unquote] in that if it occurs within a
@racket[quasiquote], it means we switch back in to
expression mode. The difference is the expression must
produce a list (or pair) and the elements of that list (or
pair) are spliced in to the outer data.

So for example, @racket[`(+ 1 ,@(map add1 '(2 3)))] is
equivalent to
@racket[(cons '+ (cons 1 (map add1 (list 2 3))))], which is
equivalent to @racket[(list '+ 1 3 4)], or
@racket['(+ 1 3 4)].

If the expression inside the @racket[unquote-splicing]
produces something other than a pair, an error is signalled.

@section{Poetry of s-expressions}

The use of structures lets us program in a style very
similar to idiomatic OCaml programming. For each variant
data type, we can define a structure type for each variant
and use pattern matching to process such values.

However, we are going to frequently employ a different idiom
for programming with recursive variants which doesn't rely
on structures, but rather uses symbols in place of
constructors and lists in place of fields.

Let's revisit the binary tree example, using this style.

Notice that @racket[leaf] structure is a kind of atomic
data. It doesn't contain anything and its only real purpose
is to be distinguishable from @racket[node] structures. On
the other hand a @racket[node] structure needs to be
distinguishable from @racket[leaf]s, but also contain 3
pieces of data within it.

We can formulate definition of binary trees using only
symbols and lists as:

@ex[
(code:comment "type BinaryTree = 'leaf | (list 'node Integer BinaryTree BinaryTree)")
]

So the following are binary trees:

@ex[
'leaf
(list 'node 3 'leaf 'leaf)
(list 'node 3
      (list 'node 7 'leaf 'leaf)
      (list 'node 9 'leaf 'leaf))
]

This formulation has the added benefit that we write binary trees
as s-expressions:

@ex[
'leaf
'(node 3 leaf leaf)
'(node 3
       (node 7 leaf leaf)
       (node 9 leaf leaf))
]

We re-write our functions to match this new datatype definition:

@ex[
(define (bt-empty? bt)
  (match bt
    ['leaf #t]
    [(cons 'node _) #f]))
(bt-empty? 'leaf)
(bt-empty? '(node 3
                  (node 7 leaf leaf)
                  (node 9 leaf leaf)))
(define (bt-height bt)
  (match bt
    ['leaf 0]
    [(list 'node _ left right)
     (+ 1 (max (bt-height left)
               (bt-height right)))]))
(bt-height 'leaf)
(bt-height '(node 3
                  (node 7 leaf leaf)
                  (node 9 leaf leaf)))]

We even can use @racket[quasiquote] notation in patterns to write
more concise definitions:

@ex[
(define (bt-empty? bt)
  (match bt
    [`leaf #t]
    [`(node . ,_) #f]))
(bt-empty? 'leaf)
(bt-empty? '(node 3
                  (node 7 leaf leaf)
                  (node 9 leaf leaf)))
(define (bt-height bt)
  (match bt
    [`leaf 0]
    [`(node ,_ ,left ,right)
     (+ 1 (max (bt-height left)
               (bt-height right)))]))
(bt-height 'leaf)
(bt-height '(node 3
                  (node 7 leaf leaf)
                  (node 9 leaf leaf)))]

Moreover, we can embrace quasiquotation at the type-level and write:

@ex[
(code:comment "type BinaryTree = `leaf | `(node ,Integer ,BinaryTree ,BinaryTree)")
]

@section{Testing, modules, submodules}

We will take testing seriously in this class.  Primarily this will
take the form of unit tests, for which we will use the
@racketmodname[rackunit] library.  To use the library, you must
@racket[require] it.

Here is a simple example:

@ex[
(require rackunit)
(check-equal? (add1 4) 5)
(check-equal? (* 2 3) 7)
]

The @racket[check-equal?] function takes two arguments (and an
optional third for a message to display should the test fail) and
checks that the first argument produces something that is
@racket[equal?] to the expected outcome given as the second argument.

There are many other forms of checks and utilities for building up
larger test suites, but @racket[check-equal?] will get us a long way.

As a matter of coding style, we will place tests nearby the function
they are testing and locate them within their own @bold{module}.
Let's talk about modules for a minute.

In Racket, a module is the basic unit of code organization.  Every
file is a module whose name is derived from the filename, but you can
also write modules without saving them in a file.  For example:

@ex[
(module bt racket
  (provide (all-defined-out))
  (struct leaf () #:prefab)
  (struct node (v l r) #:prefab)  
  (define (bt-height bt)
    (match bt
      [(leaf) 0]
      [(node _ left right)
       (+ 1 (max (bt-height left)
                 (bt-height right)))])))
]

This declares a module named @racket[bt].  It provides a single value
named @racket[bt-height].

We can require the module from the REPL to gain access to the modules
provided values:

@ex[
(require 'bt)
(bt-height (leaf))
]

We could have also used the @tt{#lang racket} shorthand for
@tt{(module bt racket ...)} and saved this in a file called
@tt{bt.rkt}.  To import from a file in the current directory, you'd
write @tt{(require "bt.rkt")}.  But this doesn't work well in REPL.

For the most part we will organize our programs into single module
files using the @tt{#lang racket} shorthand.  But we will place tests
within a ``sub''-module, i.e. a module nested inside of the module
that contains the code it tests.  We will use a special form called
@racket[module+] which declares a submodule that has access to the
enclosing module.  Moreover, repeated uses of @racket[module+] will
add content to the submodule.  By convention, we will name the testing
submodule @racket[test].

So here's a second version of the @racket[bt] module with unit tests
included (and more code).  Note the use of @racket[all-defined-out] to
provide everything:

@ex[
(module bt2 racket
  (code:comment "provides everything defined in module")
  (provide (all-defined-out))

  (module+ test
    (require rackunit))

  (struct leaf () #:prefab)
  (struct node (v l r) #:prefab)  

  (define (bt-empty? bt)
    (match bt
      [(leaf) #t]
      [_ #f]))

  (module+ test
    (check-equal? (bt-empty? (leaf)) #t)
    (check-equal? (bt-empty? (node 3
                                   (node 7 (leaf) (leaf))
                                   (node 9 (leaf) (leaf))))
                  #f))

  (define (bt-height bt)
    (match bt
      [(leaf) 0]
      [(node _ left right)
       (+ 1 (max (bt-height left)
                 (bt-height right)))]))

  (module+ test
    (check-equal? (bt-height (leaf)) 0)
    (code:comment "intentionally wrong test:")
    (check-equal? (bt-height (node 3 (leaf) (leaf))) 2)))
]

Requiring this module with make @racket[bt-height], but @emph{it will not run the tests}:

@ex[
(require 'bt2)
]

Running the tests only happens when the @racket[test] submodule is required:

@ex[
(require (submod 'bt2 test))
]

Putting it all together, we can write the following code and save it
in a file called @tt{bt.rkt}.  (You can right-click the file name and
save the code to try it out.)

@codeblock-include["bt.rkt"]

This code follows a coding style that we will use in this course:
@itemlist[
@item{it's organized in a module,}
@item{data type definitions occur at the top of the file,}
@item{it uses a test submodule to group unit tests,}
@item{tests occur immediately after the functions they test,}
@item{functions are annotated with type signatures and short purpose statements, and}
@item{indentation follows standard conventions (which DrRacket can apply for you).}
]

From the command line, you can run a module's tests using the Racket
command line testing tool @tt{raco test}:

@(shellbox "raco test bt.rkt")

Or simply give a directory name and test everything within that directory:

@(shellbox "raco test .")

@emph{These notes are adapted from @link["https://www.cs.umd.edu/class/spring2022/cmsc430/OCaml_to_Racket.html"]{CMSC430 at UMD}.}
