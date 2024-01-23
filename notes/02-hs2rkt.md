---
layout: page
parent: Notes
title: 02. From Haskell to Racket
---

# From Haskell to Racket
{: .no_toc }

<details open markdown="block">
  <summary>
    Table of contents
  </summary>
  {: .text-delta }
- TOC
{:toc}
</details>

## Basic Values

Let’s start by looking at something you know: Haskell. In Haskell, expressions can include literals for numbers, strings, booleans. Here we are using the Haskell's GHCi which provides a read-eval-print-loop (REPL) to type in examples and evaluate their results:

```haskell
-- Haskell
> 8
8
> "haskell"
"haskell"
> True
True
> False
False
```

Note, evaluating values in GHCi gives the value back. Despite Haskell being a typed language, GHCi does not print the types of the expressions by default. We can use `:set +t` to require GHCi to print the type of the expression it evaluates.

```haskell
-- Haskell
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
```

GHCi now prints the type of the expression it evaluated as `it :: Type`. Here `it` refers to the last evaluated expression. Note how `8` is not printed as an `Integer` but as `Num a => a`. It means `8` has a type `a` where `a` is an instance of the `Num` typeclass. The `Num` typeclass describes common properties of various kinds of number, such as for example. The native machine integer type `Int`, arbitrary-sized integers `Integer`, and even floating point type `Double` are instances of `Num`.

The Racket REPL also operates similarly:

```racket
;; Racket
> 8
8
> "racket"
"racket"
> #t
#t
> #f
#f
```

Racket only prints the value as it is an untyped language. The notation for booleans is slightly different, but both languages agree on numbers, strings, and booleans. The languages are essentially the same so far.

## Basic Operations

Haskell uses an infix notation for writing operations.

```haskell
-- Haskell
> 1 + 2
3
it :: Num a => a
```

The order of operations follows the usual mathematical precendence rules (which you must memorize), or you can use parentheses to indicate grouping:

```haskell
-- Haskell
> 1 + (2 * 2)
5
it :: Num a => a
> (1 + 2) * 2
6
it :: Num a => a
```

Extraneous parenthesis are fine:

```haskell
-- Haskell
> (((1))) + ((2 * 2))
5
it :: Num a => a
```

Compared to many languages you may know, including Haskell, Racket employs a uniform, minimalistic concrete syntax based on the concept of parenthesized, prefix notation.

In this notation, parentheses play a much more central role. They are not optional and they signal the form of the expression.

Languages, like people, descend from their ancestors and inherit some of their properties. In the case of notation, Racket inherits the Lisp (and Scheme) notation for programs. It takes a bit of getting used to, but once aclimated, the notation should feel lightweight and consistent; there is verry little to memorize when it comes to syntax.

So in Racket, we would write:

```racket
;; Racket
> (+ 1 (* 2 2))
5
> (* (+ 1 2) 2)
6
```

Note that there are no precendence rules for addition and multiplication: the form of the expression makes it unambiguous.

Parenthesis indicate function applications, so adding extraneous parens means something different than in Haskell:

```racket
;; Racket
> (1)
application: not a procedure;
 expected a procedure that can be applied to arguments
  given: 1
```

## Functions

Haskell also has a notation for writing functions:

```haskell
-- Haskell
> \x y -> x + y
<interactive>:18:1: error:
    * No instance for (Show (Integer -> Integer -> Integer))
        arising from a use of 'print'
        (maybe you haven't applied a function to enough arguments?)
    * In a stmt of an interactive GHCi command: print it
```

The REPL prints the values of the expression it evaluated. Once functions are evaluated, their source cannot be printed. However, we can still ask GHCi to print the type of the expression by preceeding it with a `:t`.

```haskell
-- Haskell
> :t \x y -> x + y
(\x y -> x + y) :: Num a => a -> a -> a
```

This make an anonymous function that consumes two numbers and produces their sum.

To apply it, we can write it justapoxed with arguments:

```haskell
-- Haskell
> (\x y -> x + y) 3 4
7
it :: Num a => a
```

Note that in Haskell, every function is a function of exactly one argument. Therefore `\x y -> x + y` is actuallty shorthand for `\x -> \y -> x + y`.

Applying such a function to fewer than 2 arguments will do a partial function application, which will produce a function that take the remaining arguments:

```haskell
-- Haskell
> :t (\x y -> x + y) 3
(\x y -> x + y) 3 :: Num a => a -> a
```

To encode functions that must always be given two arguments, a tuple can be used:

```haskell
-- Haskell
> :t \(x, y) -> x + y
\(x, y) -> x + y :: Num a => (a, a) -> a
```

To apply such a function, it must be given a pair of integers:

```haskell
-- Haskell
> (\(x, y) -> x + y)(3, 4)
7
it :: Num a => a
```

The use of (x, y) here in the function parameters is actually a pattern. This can be understood as shorthand for:

```haskell
-- Haskell
> :t \p -> case p of (x, y) -> x + y
\p -> case p of (x, y) -> x + y :: Num a => (a, a) -> a
```

So even this function is actually taking a single argument (which must be a pair of numbers).

Racket has a similar notation for writing functions:

```racket
;; Racket
> (λ (x) (λ (y) (+ x y)))
#<procedure>
```

You can also write this without the fancy `λ` by spelling it `lambda`:

```racket
;; Racket
> (lambda (x) (lambda (y) (+ x y)))
#<procedure>
```

(In DrRacket, to insert a `λ` press Cmd + \)

To apply it, it must be written in parens, juxtaposed with arguments:

```racket
;; Racket
> (((λ (x) (λ (y) (+ x y))) 3) 4)
7
```

Functions in Racket do not always consume a single argument. They can consume 0, 1, or more arguments.

```racket
;; Racket
> (λ (x y) (+ x y))
#<procedure>
```

This is not a shorthand for the function above it; rather it is a function that expects two arguments:

```racket
;; Racket
> ((λ (x y) (+ x y)) 3 4)
7
```

Applying a function to the wrong number of arguments will result in an error (and not perform partial function application):

```racket
;; Racket
> ((λ (x y) (+ x y)) 3)
arity mismatch;
 the expected number of arguments does not match the given number
  expected: 2
  given: 1
```

## Definitions

In Haskell, variables can be defined with `let`:

```haskell
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
```

The `:{` and `:}` marks the start and end of a multi-line definition in the REPL.

In Racket, variables are defined with the `define` form:

```racket
> (define x 3)
> (define y 4)
> (+ x y)
7

> (define fact
    (λ (n)
     (match n
       [0 1]
       [n (* n (fact (- n 1)))])))
> (fact 5)
120
```

In Haskell, function definitions can be written as:

```haskell
let fact n = case n of
                  0 -> 1
                  n -> n * (fact (n - 1))
```

This is just a shorthand for the definition written above in terms of `\`.

Similarly in Racket, function definitions can be written as:

```racket
> (define (fact n)
    (match n
      [0 1]
      [n (* n (fact (- n 1)))]))
```

which is shorthand for the definition above using λ.

Notice both Haskell and Racket have pattern matching forms, which are quite useful for writing function in terms of a number of "cases." More on this in a minute.

## Lists

Haskell has a built-in list datatype. The empty list is written `[]` and `:` is an operation for “consing” an element on to a list. So to build a list with three integer elements, 1, 2, and 3, you’d write:

```haskell
> 1 : 2 : 3 : []
[1,2,3]
it :: Num a => [a]
```

Racket has a built-in list datatype. The empty list is written `'()` and `cons` is an operation for consing an element on to a list. To build the same list, you’d write:

```racket
> (cons 1 (cons 2 (cons 3 '())))
'(1 2 3)
```

The notation `(list 1 2 3)` is shorthand for the above.

There is a slight difference here. For one, Haskell lists must be homogeneous. You can have a list of strings or a list of numbers, but you can’t have a list of strings and numbers.

```haskell
> ["a", 3]
<interactive>:45:7: error:
    * No instance for (Num [Char]) arising from the literal '3'
    * In the expression: 3
      In the expression: ["a", 3]
      In an equation for 'it': it = ["a", 3]
```

In Racket, there is no such restriction:

```racket
> (list "a" 3)
'("a" 3)
```

Also, in Racket, `cons` plays the role of both tupling (making pairs) and making lists (making a pair of an element and another list).

So in Haskell, you could make a pair `("a", 3)`. In Racket, you’d write `(cons "a" 3)`. Note this is a pair and not a proper list. In Haskell, tuples and lists are disjoint things. In Racket, lists and tuples (pairs) are made out of the same stuff.

This can be confusing the first time you encounter it, so let’s go over it a bit more.

In Racket (or any Lisp), `cons` plays the role of both the pair constructor and the list constructor. Non-empty lists are a subset of pairs: they are pairs whose second component is a list (either the empty list or another pair whose second component is a list, etc.).

You can make pairs out of any kind of element and you can make lists out of any kind of elements. We can precisely define these sets as:

```racket
;; type ListofAny =
;; | '()
;; | (cons Any ListofAny)
;; type PairofAny =
;; | (cons Any Any)
```

Or, to give more useful parameterized definitions:

```racket
;; type (Listof A) =
;; | '()
;; | (cons A (Listof A))
;; type (Pairof A B) =
;; | (cons A B)
```

The functions `first` and `rest` operate on non-empty lists, producing the first element of the list and the tail of the list, respectively.

```racket
> (first (cons 3 (cons 4 '())))
3
> (rest (cons 3 (cons 4 '())))
'(4)
```

These function will produce errors if given something that is a pair but not a list:

```racket
> (first (cons 3 4))
first: contract violation
  expected: (and/c list? (not/c empty?))
  given: '(3 . 4)
> (rest (cons 3 4))
rest: contract violation
  expected: (and/c list? (not/c empty?))
  given: '(3 . 4)
```

On the other hand, the functions `car` and `cdr` access the left and right components of a pair (the names are admittedly awful and an artifact of Lisp history):

```racket
> (car (cons 3 4))
3
> (cdr (cons 3 4))
4
```

When given pairs that are also lists, they behave just like `first` and `rest`:

```racket
> (car (cons 3 (cons 4 '())))
3
> (cdr (cons 3 (cons 4 '())))
'(4)
```

## Pattern Matching

Haskell has a very nice pattern matching for letting you express case analysis and decomposition in a concise way.

Each pattern maching expression has a sub-expression that produce a value to be matched against and a number of clauses. Each clause has a pattern and an expression. The pattern potentially consists of data constructors, variables, and literals. If the value matches the first pattern, meaning the value and the template match up on constructors and literals, then the variables are bound to the correspond parts of the value, and the right-hand side expression is evaluated. If the value doesn’t match, the next pattern is tried, and so on. It’s an error if none of the patterns match.

So for example, we can write a function that recognize even digits as:

```haskell
> :{
  let evenDigit n = case n of 0 -> True
                              2 -> True
                              4 -> True
                              6 -> True
                              8 -> True
                              _ -> False
  :}
evenDigit :: (Eq a, Num a) => a -> Bool
```

The patterns here, save the last one, are just integer literals. If `n` is the same as any of these integers, the value `True` is produced. The last case uses a "wildcard," which matches anything and produces `False`.

Here’s an example that matches a tuple, binding each part of the tuple to a name and then using those names to construct a different tuple:

```haskell
> let swap p = case p of (x, y) -> (y, x)
swap :: (b, a) -> (a, b)
```
Here the pattern uses a data constructor (the tuple constructor). It matches any value that is made with the same constructor.

Here is a recursive function for computing the sum of a list of numbers, defined with pattern matching:

```haskell
> :{
let addNums xs = case xs of []   -> 0
                            x:xs -> x + (addNums xs)
:}
addNums :: Num p => [p] -> p
> addNums [4,5,6]
15
it :: Num p => p
```

We can do the same in Racket:

```racket
> (define (even-digit n)
    (match n
      [0 #t]
      [2 #t]
      [4 #t]
      [6 #t]
      [8 #t]
      [_ #f]))
> (define (swap p)
    (match p
      [(cons x y) (cons y x)]))
> (define (sum xs)
    (match xs
      ['() 0]
      [(cons x xs)
       (+ x (sum xs))]))
> (sum (list 4 5 6))
15
```

## Datatypes

Haskell has the ability to declare new datatypes. For example, we can define type for binary trees of numbers:

```haskell
data BinaryTree = Leaf
                | Node Integer BinaryTree BinaryTree
```

This declares a new type, named `BinaryTree`. There are two variants of the `BinaryTree` type, each with their own constructor: `Leaf` and `Node`. The `Leaf` constructor takes no arguments, so just writing `Leaf` creates a (empty) binary tree:

```haskell
> :t Leaf
Leaf :: BinaryTree
```

The `Node` constructor takes three arguments: an integer and two binary trees. Applying the constructor to a tuple of three things, makes a (non-empty) binary tree:

```haskell
> :t Node 3 Leaf Leaf
Node 3 Leaf Leaf :: BinaryTree
```

Binary trees are an example of a recursive datatype, since one of the variants contains binary trees. This means we can build up arbitrarily large binary trees by nesting nodes within nodes:

```haskell
> :t Node 3 (Node 4 Leaf Leaf) (Node 7 Leaf Leaf)
Node 3 (Node 4 Leaf Leaf) (Node 7 Leaf Leaf) :: BinaryTree
```

Pattern matching is used to do case analysis and deconstruct values. So for example, a function that determines if a binary tree is empty can be written as:

```haskell
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
```

The patterns use the constructor names to discriminate on which constructor was used for a given binary tree. The use of the wildcard here is just saying it doesn’t matter what’s inside a node; if you’re a node, you’re not empty.

Recursive functions work similarly, but use variables inside patterns to bind names to the binary trees contained inside a node:

```haskell
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
```

We do something very similar in Racket using structures. A structure type is like a (single) variant of a data type in Haskell: it’s a way of combining several things into one new kind of value.

```racket
> (struct leaf ())
> (struct node (i left right))
```

This declares two new kinds of values: leaf structures and node structures. For each, we get a constructor, which is a function named after the structure type. The `leaf` constructor takes no arguments. The node constructor takes 3 arguments.

```racket
> (leaf)
(leaf)
> (node 5 (leaf) (leaf))
(node 5 (leaf) (leaf))
> (node 3 (node 2 (leaf) (leaf)) (leaf))
(node 3 (node 2 (leaf) (leaf)) (leaf))
```

There is no type system in Racket, but we can conceptually still define what we mean in a comment. Just like in Haskell, we can use pattern matching to discriminate and deconstruct:

```racket
;; type BinaryTree = (leaf | (node Integer BinaryTree BinaryTree))
> (define (bt-empty? bt)
    (match bt
      [(leaf) #t]
      [(node _ _ _) #f]))
> (bt-empty? (leaf))
#t
> (bt-empty? (node 5 (leaf) (leaf)))
#f
> (define (bt-height bt)
    (match bt
      [(leaf) 0]
      [(node _ left right)
       (+ 1 (max (bt-height left)
                 (bt-height right)))]))
> (bt-height (leaf))
0
> (bt-height (node 4 (node 2 (leaf) (leaf)) (leaf)))
2
```

---

_These notes are adapted from [CMSC430 at UMD](https://www.cs.umd.edu/class/spring2022/cmsc430/OCaml_to_Racket.html)._
