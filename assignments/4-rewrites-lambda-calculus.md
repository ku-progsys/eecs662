---
layout: page
parent: Assignments
title: 4. Term Rewrites and Lambda Calculus
---

# Term Rewrites and Lambda Calculus

You are given a zip file on Canvas `hw4.zip` with a starter code for the assignment.

The next two tasks require you to modify a `rewriter.rkt` file. These two tasks are based on the language we developed in class, but simplified further. This language does not have top-level definitions, only lambda functions. You may write any auxillary functions in order to solve the given tasks.

## Rewrite `cond` to sequence of `if`

In [Assignment 2]({{site.baseurl}}/assignments/2-moreprim-cond/), you added `cond` which enabled you to write a sequence of conditionals and evaluate them sequentially. To do it, you added AST nodes, parsed the concrete syntax, and added relevant semantics to the interpreter. The goal of this task is to rewrite `cond` statement to nested `if` expressions. For example:

```racket
(cond [(zero? (- 6 5)) 1]
      [(<= 6 7)        2]
      [else            3])
```

can be written as

```racket
(if (zero? (- 6 5))
    1
    (if (<= 6 7)
        2
        3))
```

More generally,

```racket
(cond [e-p1 e-a1]
      [e-p2 e-a2]
      [e-p3 e-a3]
      ...
      [else e-an])
```

can be written as

```racket
(if e-p1
    e-a1
    (if e-p2
        e-a2
        (if e-p3
            e-a3
            ... e-an)))
```

The benefit of rewriting expressive syntax to a simpler term automatically enables us to keep our interpreter simple, reducing the chances of bugs. To do this you must:

* Write a function `cond->if` in `rewriter.rkt`. It takes a parsed expression as an input and produces the rewritten expression with `cond` replaced by `if`.
* There is no need to modify `ast.rkt`, `parser.rkt`, `interp.rkt`.
* Add tests in `rewriter.rkt` to check if you handle rewrites correctly in all kinds of expressions.

## Currify functions

In functional programming, currying is a technique of translating a function that takes multiple arguments into a series of functions that each take a single argument. This transformation enables more flexible function composition and partial application. Haskell is a language that supports partial application. For example, if we have a function `adder` that adds 2 numbers:

```racket
(let ((adder (λ (x y)
               (+ x y))))
  (adder 2 3))
```

A currified version of the same code will look like:

```racket
(let ((adder (λ (x) (λ (y)
                      (+ x y)))))
  ((adder 2) 3))
```

A big advantage of a language that allows currying is to enable partial application:

```racket
(let ((adder (λ (x) (λ (y)
                      (+ x y)))))
  (let ((adder2 (adder 2)))
    (adder2 5)))
```

The function `adder2` is built using `adder`, but takes one argument and always adds `2` to it. One can create such partially applied functions and can apply it on rest of the arguments later in the program. The goal of this task is to rewrite lambdas and their applications to the currified version. For example:

```racket
(let ((adder (λ (x y)
               (+ x y))))
  (adder 2 3))
```

will be transformed to:

```racket
(let ((adder (λ (x) (λ (y)
                      (+ x y)))))
  ((adder 2) 3))
```

In general,

```racket
(λ (x-1 x-2 ... x-n) e)
```

is converted to:

```racket
(λ (x-1) (λ (x-2) ... (λ (x-n) e)))
```

Similarly, function applications are also transformed as:

```racket
(e-1 e-2 e-3 ... e-n)
```

to 

```racket
(((e-1 e-2) e-3) ... e-n)
```

Doing such a rewrite will allow our language to support partial application without making any changes to the interpreter.

* Write a function `currify` in `rewriter.rkt`. It takes a parsed expression as an input and produces the rewritten currified expression.
* There is no need to modify `ast.rkt`, `parser.rkt`, `interp.rkt`.
* Add tests in `rewriter.rkt` to check if you handle rewrites correctly in all kinds of expressions.

---

The next 3 tasks require you to modify a `lambda-calculus.rkt` file. This does not depend on the existing languages we have developed till now though it is very similar. This file does not depend on any other files (like `ast.rkt`, `parser.rkt`, `rewriter.rkt`, `interp.rkt`). It implements only the core lambda calculus, i.e., it has variables, function abstraction (lambdas), and function application. The next 3 tasks are self-contained within this file and meant to give you an idea of how lamba terms are handled. You should only modify the `lambda-calculus.rkt` file for these tasks. You may write any auxillary functions in order to solve the given tasks.

## Query a lambda term for free variables

Write a function `free?` that takes these arguments in the given order:

* A list of bound variables
* The identifier to check if it is free or bound
* The expression to check this in

You can find more information about free and bound variables [here]({{site.baseurl}}/notes/11-lambda-calculus/#free-and-bound-variables).

For example:

```racket
; x is a free is the first sub-expression
> (free? '() 'x (parse '(x ((λ (x) x) y))))
#t

; x is considered bound for the same expression because x is present in the bound variables list
> (free? '(x) 'x (parse '(x ((λ (x) x) y))))
#f

; z is the free variable, not x
> (free? '()  'x (parse '(z ((λ (x) x) y))))
#f
```

## Alpha reduce a lambda term

Write a function `alpha-reduce` that takes these arguments in the given order:

* The parsed expression to alpha rename
* The identifier to rename from
* The identifier to rename to (you may assume this is always fresh)

You can find more information and the algorithm for alpha-reduction [here]({{site.baseurl}}/notes/11-lambda-calculus/#alpha-reductions).

```racket
; renames all free x to z
> (unparse (alpha-reduce (parse '(λ (y) x)) 'x 'z))
'(λ (y) z)

; renames all free x to the variable generated by gensym
> (unparse (alpha-reduce (parse '(λ (y) x)) 'x (gensym)))
'(λ (y) g1956706)

; does not rename bound variables
> (unparse (alpha-reduce (parse '(λ (y) x)) 'y (gensym)))
'(λ (y) x)
```

## Beta reduce a lambda term

Write a function `beta-reduce` that takes these arguments in the given order:

* The parsed expression to beta reduce
* The identifier to be substituted
* The parsed expression with which it will be substituted

You can find more information and the algorithm for alpha-reduction [here]({{site.baseurl}}/notes/11-lambda-calculus/#beta-reductions).

```racket
; x is substituted with (λ (x) (x x)) in (x x)
> (unparse (beta-reduce (parse '(x x)) 'x (parse '(λ (x) (x x)))))
'((λ (x) (x x)) (λ (x) (x x)))

; x is substituted with y in (λ (y) x), and the renamed variable here does not matter
> (unparse (beta-reduce (parse '(λ (y) x)) 'x (parse 'y)))
'(λ (g1993789) y)
```

## Testing

You should test your code by writing test cases and adding them to relevant files. Use the command raco test [filename] to test your code. Alternatively, pressing “Run” in Dr. Racket will also run your test cases. There are a lot of cases in this assignment, so test your code carefully.

For grading, your submitted interpreter will be tested on multiple inputs that should work. Writing your own test cases will give you confidence that your code can handle previously unseen expressions.

## Submitting

Submit your work on GradeScope. You should submit all Racket files: `ast.rkt`, `parser.rkt`, `interp.rkt`, `rewriter.rkt`, and `lambda-calculus.rkt`. You may add any auxiliary functions you need to these files, but do not rename the functions you are asked to write.
