---
layout: page
parent: Assignments
title: 2. More Primitives and Conditionals
---

# More Primitives and Conditionals

The goal of this assignment is to extend the parser and interpreter of the Con language with some features. We will call this language _Con+_. This will require you to change the language in a similar fashion as we have been doing in class.

You are given a zip file on Canvas `hw2.zip` with a starter code for the assignment. This is same as the code for the [Con]({{site.baseurl}}/notes/06-booleans/) language from class. You should add your own test cases to ensure your submission is correct. Your tasked with extending the language to Con+ in the following ways:

* Add few unary and binary primitive operations in the language,
* Add conditional evaluation with `cond`

You have to submit one language implementation with all these changes, and not 2 languages.

## New primitives

Add the following forms of expression to the language:

* `(or e e)`: `(or e1 e2)` means `e1` if `e1` does not mean `#f`, else `(or e1 e2)` means the same as `e2`.
* `(- e)`: flips the sign of `e`, i.e., computes `(- 0 e)`.
* `(not e)`: compute the logical negation of `e`. Negation of `#f` is `#t`, negation of `#t` is `#f`, and the negation of any other value is `#f`.
* `(% e e)`: `(% e1 e2)` means the remainder when dividing `e1` by `e2`. The Racket builtin [`remainder`](https://docs.racket-lang.org/reference/generic-numbers.html#%28def._%28%28quote._~23~25kernel%29._remainder%29%29) will be useful.

To do this, you should:

* Extend `parser.rkt` to add support for parsing these programs;
* Extend `interp.rkt` to add the semantics for these expressions;
* And, add tests in `interp.rkt` and run it to see if your programs reflect the above semantics.

## Conditional evaluation with `cond`

The Con language has a simple form of performing conditional evaluation of subexpressions:

```racket
(if e1 e2 e3)
```

However, in the original paper on Lisp, [Recursive Functions of Symbolic Expressions and Their Computation by Machine, Part I](http://jmc.stanford.edu/articles/recursive.html), John McCarthy introduced a generalization of `if` called “conditional expressions,” which we could add to our language with the following syntax:

```racket
(cond [e-p1 e-a1]
      [e-p2 e-a2]
      ...
      [else e-an])
```

A cond expression has any number of clauses `[e-pi e-ai]` ..., followed by an "else" clause `[else en]`. For the purposes of this assignment, we will assume every `cond` expression ends in an `else` clause, even though this is not true in general for Racket. The parser should reject any `cond`-expression that does not end in `else`.

The meaning of a `cond` expression is computed by evaluating each expression `e-pi` in order until the first one that does not evaluate to `#f` is found, in which case, the corresponding expression `e-ai` is evaluated and its value is the value of the `cond` expression. If no such `e-pi` exists, the expression `e-an`’s value is the value of the `cond`.

Let us understand this with an example:

```racket
(cond [(zero? (- 6 5)) 1]
      [(<= 6 7)        2]
      [else            3])
```

The above program first checks if `(zero? (- 6 5)`. As this evaluates to `#f`, it goes to the 2nd conditional expression `(<= 6 7)`. As this condition evaluates to `#t`, the entire meaning of this `cond` expression is `2` and no further conditional arms (including `else`) are evaluated. The `else` branch is evaluated if no other condition holds.

To implement this, you should:

* Study `ast.rkt` to add appropriate AST nodes;
* Extend `parser.rkt` to parse such expressions;
* Update `interp.rkt` to correctly interpret `cond` expressions;
* And, add tests in `interp.rkt` and run it to see if your programs reflect the semantics of `cond`.

## Testing

You should test your code by writing test cases and adding them to relevant files. Use the command `raco test [filename]` to test your code. Alternatively, pressing "Run" in Dr. Racket will also run your test cases.

For grading, your submitted interpreter will be tested on multiple programs drawn from this language. Writing your own test cases will give you confidence that your interpreter can handle previously unseen programs.

## Submitting

You should submit on Canvas. You should submit a zip file with exactly the same structure (a con-plus folder). We will only use the `ast.rkt`, `parser.rkt`, and `interp.rkt` files for grading, so make sure all your work is contained there! You may add any function you need to these files, but **do not** rename the `parse` and `interp` functions.
