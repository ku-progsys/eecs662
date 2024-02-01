---
layout: page
parent: Notes
title: "04. Arithmetic: Calculating Numbers"
---

# Arithmetic: Calculating Numbers
{: .no_toc }

<details open markdown="block">
  <summary>
    Table of contents
  </summary>
  {: .text-delta }
- TOC
{:toc}
</details>

## Refining the Amount Language

We have seen all the essential pieces of a language (a grammar, an AST data type definition, operational sematics, an interpreter, and some correctness tests) for implementing a programming language, although for a very simple one.

We will now, through the process of **iterative refinement** grow the language to be more useful by designing more features.

We will add arithmetic operations to Amount, such that we can now increment, decrement numbers as well as do arithmetic operations such as addition, subtraction, multiplication, and division. We will call this new language Arithmetic. It is still a very simple language, but atleast our programs will compute something.

## Concrete syntax for Arithmetic

Arithmetic extends Amount to have some unary and binary operations. It contains the `#lang racket` line followed by a single expression. The grammar of concrete expressions is:

<!-- 
\begin{array}{lccl}
\textrm{Expressions} & e & ::= & \textrm{Integer} \mid \texttt{(add1 $e$)} \mid \texttt{(add1 $e$)} \\
                     &   & \mid & \texttt{(+ $e$ $e$)} \mid \texttt{(- $e$ $e$)} \mid \texttt{(* $e$ $e$)} \mid \texttt{(/ $e$ $e$)}
\end{array}
 -->
![Arithmetic Grammar]({{site.baseurl}}/images/arithmetic-grammar.png)

So `42`, `-8`, `120` are all valid programs now. But so are programs like `(add1 42)`, `(+ 43 (add1 23))`.

An example of a concrete program:

```racket
#lang racket
(+ 43 (- (add1 23) (sub1 -8)))
```

## Abstract syntax for Arithmetic

The grammar of abstract Arithmetic expressions are:

<!-- 
\begin{array}{lccl}
\textrm{Expressions} & e & ::= & \texttt{(Int $i$)} \mid \texttt{(UnOp $u$ $e$)} \mid \texttt{(BinOp 
$b$ $e$ $e$)} \\
                     & i & ::= & \textrm{Integer} \\
\textrm{Unary Ops}   & u & ::= & \texttt{'add1} \mid \texttt{'sub1} \\
\textrm{Binary Ops}  & b & ::= & \texttt{'+} \mid \texttt{'-} \mid \texttt{'*} \mid \texttt{'/} \\
\end{array}
 -->
![Arithmetic AST]({{site.baseurl}}/images/arithmetic-ast.png)

So just like `(Int 42)`, `(Int -8)`, and `(Int 120)` are all valid ASTs, so are `(UnOp 'add1 (Int 42))` and `(BinOp '+ (Int 43) (UnOp 'add1 (Int 23)))`.

We can define these data types for representing expressions as:

```racket
#lang racket
;; arithmetic/ast.rkt

(provide Int UnOp BinOp)
 
;; type Expr = 
;; | (Int Integer)
;; | (UnOp u Expr)
;; | (BinOp b Expr Expr)
;;
;; type UnOp = 'add1 | 'sub1
;; type BinOp = '+ | '- | '* | '/

(struct Int (i) #:prefab)
(struct UnOp (u e) #:prefab)
(struct BinOp (b e1 e2) #:prefab)
```

The parser is more involved that Amount, but still straightforward:

```racket
#lang racket
;; arithmetic/parse.rkt

(provide parse)
(require "ast.rkt")
 
;; S-Expr -> Expr
(define (parse s)
  (match s
    [(? integer?) (Int s)]
    [(list (? unop? o) e) (UnOp o (parse e))]
    [(list (? binop? o) e1 e2) (BinOp o (parse e1) (parse e2))]
    [_            (error "Parse error")]))

;; Any -> Boolean
(define (unop? x)
  (memq x '(add1 sub1)))

;; Any -> Boolean
(define (binop? x)
  (memq x '(+ - * /)))
```

## Meaning of Arithmetic Programs

The meaning of Arithmetic programs depends of the form of the expression:

* The meaning of the integer literal is the just the integer itself;
* The meaning of the the increment expression (`add1`) is one more than the subexpression;
* The meaning of the the decrement expression (`sub1`) is one less than the subexpression;
* The meaning of the the addition expression (`+`) is the sum of two subexpressions;
* The meaning of the the subtraction expression (`-`) is the difference between the first and second subexpressions;
* The meaning of the the multiplication expression (`*`) is the product of the two subexpressions;
* The meaning of the the division expression (`/`) is the result of dividing the first subexpression by the second.

Note, that Arithmetic is a language of integers, hence all operations should evaluate to integers.

The operational sematics reflects the dependence of the subexpressions as well.

As the meaning of integers do not depend on a subexpression:

![Arithmetic Int]({{site.baseurl}}/images/numbers-opsem.png)

The meaning of the increment operation depends on only one subexpression:

<!--
\frac{e \Downarrow i}{\texttt{(UnOp 'add1 $e$)} \Downarrow i + 1} \qquad [\textsc{E-Incr}]
-->
![Arithmetic Increment]({{site.baseurl}}/images/arithmetic-add1.png)

The meaning of the addition operation depends on two subexpression:

<!--
\frac{e_1 \Downarrow i_1 \quad e_2 \Downarrow i_2}{\texttt{(BinOp '+ $e_1$ $e_2$)} \Downarrow i_1 + i_2} \qquad [\textsc{E-Add}]
-->
![Arithmetic Add]({{site.baseurl}}/images/arithmetic-add.png)

The first rule looks familiar; it's the exact semantics of integers from Amount. The second and thirs rule are more involved. In particular they have **premises** over the line. If the premise is true, the **conclusion** below the line is true as well. These rules are conditional on the premise being true. In contrast, the first rule applies unconditionally.

The rules for `sub1`, `-`, `*`, and `/` will look similar. You should try writing those down as an exercise.

Recall that ![]({{site.baseurl}}/images/downarrow.png) is a relation. So we can say:

* For all integers _i_, `(Int i)` is in ![]({{site.baseurl}}/images/downarrow.png);
* For all expressions _e_ and integers _i_,  if `(e, i)` is in ![]({{site.baseurl}}/images/downarrow.png) then `((UnOp 'add1 e), i + 1)` is in ![]({{site.baseurl}}/images/downarrow.png);
* For all expressions _e1_, _e2_ and integers _i1_ and _i2_,  if `(e1, i1)` and `(e2, i2)` are in ![]({{site.baseurl}}/images/downarrow.png) then `((BinOp '+ e1 e2), i1 + i2)` is in ![]({{site.baseurl}}/images/downarrow.png);
* ... and so on!

These rules are inductive. We start from the meaning of integers and if we have the meaning of an expression, we can construct the meaning of a larger expression.

## Intepreter for Arithmetic

We can translate these operational semantics rules to an interpreter:

```racket
#lang racket

(provide interp)

(require "ast.rkt")

;; Expr -> Integer
(define (interp e)
  (match e
    [(Int i) i]
    [(UnOp op e) (interp-unop op (interp e))]
    [(BinOp op e1 e2) (interp-binop op (interp e1) (interp e2))]))

;; Op Integer -> Integer
(define (interp-unop op i)
  (match op
    ['add1 (add1 i)]
    ['sub1 (sub1 i)]))

;; Op Integer Integer -> Integer
(define (interp-binop op i1 i2)
  (match op
    ['+ (+ i1 i2)]
    ['- (- i1 i2)]
    ['* (* i1 i2)]
    ['/ (/ i1 i2)]))
```

Examples:

```racket
> (interp (Int 42))
42
> (interp (Int -8))
-8
> (interp (UnOp 'add1 (Int 30)))
31
> (interp (BinOp '* (Int 2) (Int 3)))
6
> (interp (BinOp '+ (Int 43) (UnOp 'add1 (Int 23))))
67
```

Here’s how to connect the dots between the semantics and interpreter: the interpreter is computing, for a given expression _e_, the integer _i_, such that `(e, i)` is in ![]({{site.baseurl}}/images/downarrow.png). The interpreter uses pattern matching to determine the form of the expression, which determines which rule of the semantics applies.

* if _e_ is an integer `(Int i)`, then we’re done: this is the right-hand-side of the pair `(e, i)` in ![]({{site.baseurl}}/images/downarrow.png).
* if _e_ is an expression `(UnOp 'add1 e)`, then we recursively use the interpreter to compute _i_ such that `(e, i)` is in ![]({{site.baseurl}}/images/downarrow.png). But now we can compute the right-hand-side by adding `1` to _i_.
* if _e1_ and _e2_ are expressions `(BinOp '+ e1 e2)`, then we recursively use the interpreter to compute _i1_ and _i2_ such that `(e1, i1)` and `(e2, i2)` are in ![]({{site.baseurl}}/images/downarrow.png). But now we can compute the right-hand-side by adding _i1_ to _i2_.

This explanation of the correspondence is essentially a proof by induction of the interpreter’s correctness:

Interpreter Correctness: For all Blackmail expressions e and integers i, if (e,i) in image, then (interp e) equals i.

**Interpreter Correctness:** _For all expressions `e` and integers `i`, if `e` ![]({{site.baseurl}}/images/downarrow.png) `i`, then the interpreter `(interp e)` equals `i`._

## Correctness

We can turn the examples we have above into automatic test cases to verify our interpreter is correct. We will reuse the `chec-interp` function from Amount.

```racket
> (define (check-interp e)
    (check-eqv? (interp (parse e))
                (eval e)))
```

To turn this into an automatic test case:

```racket
(module+ test
  (check-interp 42)
  (check-interp -8)
  (check-interp '(add1 30))
  (check-interp '(* 2 3))
  (check-interp '(+ 43 (add1 23))))
```

The problem, however, is that generating random Arithmetic programs is less obvious compared to generating random Amount programs (i.e. random integers). Randomly generating programs for testing is its own well studied and active research area. To side-step this wrinkle, here is a small utility for generating random Amount programs, which you can use, without needing the understand how it was implemented. Don't worry, you will not be asked to write programs like this in the exam or assignments.

```racket
(define (random-expr)
   (contract-random-generate
    (flat-rec-contract b
                       (list/c 'add1 b)
                       (list/c 'sub1 b)
                       (list/c '+ b b)
                       (list/c '- b b)
                       (list/c '* b b)
                       (list/c '/ b b)
                       (integer-in #f #f))))
```

Calling `(random-expr)` now will produce random programs from our grammar:

```racket
> (random-expr)
'(/ (- (- (+ 17 1) (+ 2 -2)) (+ (* 1 -5) (sub1 -4))) (sub1 0))
> (random-expr)
'(add1 (sub1 -3))
> (random-expr)
'(sub1 (/ (- (/ -2 0) (+ 5 1)) (/ (/ 4 1) 2)))
> (random-expr)
'(* (* (* 4 (* 4 -8)) 5) (* (add1 (- -2 2)) (+ 2 (sub1 -3))))
> (random-expr)
'(add1 (add1 (+ 0 (add1 3))))
```

You can run this in a loop to check if our Arithmetic language interpreter complies with Racket semantics:

```racket
> (for ([i (in-range 100)])
    (check-interp (random-expr)))
```

At this point we have not found any counter-example to interpreter correctness. It’s tempting to declare victory. But... can you think of a valid input (i.e. some program) that might refute the correctness claim?

Think on it.
