---
layout: page
parent: Assignments
title: 3. Let bindings and arity checking
---

# Let bindings and arity checking

The goal of this assignment is to extend the AST, parser, and interpreter of the Lambda language with some features. We will call this language _Lambda+_. This will require you to change the language in a similar fashion as we have been doing in class.

You are given a zip file on Canvas `hw3.zip` with a starter code for the assignment. This is similar to the code for the Lambda language from class. You should add your own test cases to ensure your submission is correct. Your tasked with extending the language to Lambda+ in the following ways:

* Add the general form of `let` bindings
* Add a `let*` form to the language
* Arity checking for function arguments

You have to submit one language implementation with all these changes, and not 3 languages.

## Notes about the starter code

The starter code for this assignment has been refactored. Specifically these are the changes:

* Definitions and environments in the code are now called `D` and `E` reflecting the notation used in class notes.
* All the tests have been moved from the `interp.rkt` to the `tests.rkt` file. To run the tests now run the `tests.rkt` file in Dr. Racket or run `raco test tests.rkt` from your terminal.
* There are 3 tests for the three tasks in this assignment. All 3 tests should fail. If you have implemented all these features correctly, these should pass. These tests are _not complete_ and you should add more tests of your own to `tests.rkt` to ensure your implementation works.

## General form of `let` bindings

The `let` bindings we discussed in class were a simplification of the general form of `let`. The general form of let bindings allow you to declare multiple variables _at once_. An example program that binds two variables:

```racket
(let ((x 1)
      (y 2))
  (+ x y))
```

The general form of `let` can bind identifiers `id0` through `idn` to values of expressions `e0` through `en`, locally for the expression `e`:

```racket
(let ((id0 e0)
      (id1 e1)
      ...
      (idn en))
  e)
```

The meaning of a let expression `(let ((id0 e0) ... (idn en)) e)` is the meaning of `e` (the body of the `let`) when variables `id0` through `idn` means the value of `e0` through `en` respectively. The `id`s are bound "in parallel." That is, no identifier is bound in the right-hand side expression for any id, but all are available in the body. The ids must be different from each other. Here is an example to explain this:

```racket
(let ((x 1)
      (y (add1 x)))
  (+ x y))
```

The above results in an error, as declaring `y` requires the `x` to be defined, but all the newly declared bindings are only available in the body. Thus `(+ x y)` can be evaluated, but the second binding expression is invalid in this let form.

This `let` does not allow same identifier to be declared more than once. So programs like below result in an error:

```racket
(let ((x 1)
      (x (add1 x)))
  (add1 x))
```

Implement this general form of let. You may need to modify the `ast.rkt`, `parser.rkt`, and `interp.rkt` files.

## Add `let*`

Just like `let` allows variables to be declared "in parallel", the `let*` form allows you declare variables "in sequence". Thus, programs like:

```racket
(let* ((x 1)
       (y 2))
  (+ x y))
```

mean the same if they were written with `let`. However, `let*` supports latter declarations to be defined in terms of earlier declarations like:

```racket
(let* ((x 1)
       (y (add1 x)))
  (+ x y))
```

The above program is valid, and evaluates to `3`.

The general form of `let*` can bind identifiers `id0` through `idn` to values of expressions `e0` through `en`, locally for the expression `e`:

```racket
(let* ((id0 e0)
       (id1 e1)
       ...
       (idn en))
  e)
```

The meaning of a let expression `(let* ((id0 e0) ... (idn en)) e)` is the meaning of `e` (the body of the `let`) when variables `id0` through `idn` means the value of `e0` through `en` respectively. The difference from `let` is that each identfier is available for use in later expressions, as well as in the body. Furthermore, the `id`s need not be distinct, and the most recent binding is the one that is used.

In the following example:

```racket
(let* ((x 1)
       (x (add1 x)))
  (* x 2))
```

first `x` is bound to `1`. This is used to compute `(add1 x)`, i.e., `2` which is bound to the `x` again. Finally, this new value of `x` is used to compute the body `(* x 2)` which evaluates to `4`.

Implement this form of `let*`. You may need to modify the `ast.rkt`, `parser.rkt`, and `interp.rkt` files.

## Arity checking for function arguments

If a function has _n_ formal parameters, it can only be applied to _n_ arguments. The number of arguments to a function is called its arity. If they mismatch it results in an error. Consider the example:

```racket
(define (foo a b)
  (+ a b))

(foo 1)
```

will result in an error, as the function `foo` takes 2 arguments, but only 1 is provided.

The same thing applies for lambda functions. The following should also result in an error:

```racket
((λ (x y) (* x y)) 1)
```

In our implementation, we did not check for function arity when applying the function, which means our implementation might crash in unexpected ways. For this task, implement a check for function arity, i.e., check if the number formal arguments and actual arguments are same and only then evaluate the body of the function. If they are different, raise an `Err` value stating arity mismatch. You can use the Racket provided function `length` to find the length of lists.

Remember, there are 2 places functions are called: function definitions and lambdas.

## Testing

You should test your code by writing test cases and adding them to the `tests.rkt` file. Use the command `raco test tests.rkt` to test your code. Alternatively, pressing "Run" in Dr. Racket will also run your test cases.

For grading, your submitted interpreter will be tested on multiple programs drawn from this Lambda+ language. Writing your own test cases will give you confidence that your interpreter can handle previously unseen programs.

## Submitting

You should submit on Canvas. You should submit a zip file with exactly the same structure (a lambda-plus folder). We will only use the `ast.rkt`, `parser.rkt`, and `interp.rkt` files for grading, so make sure all your work is contained there! You may add any function you need to these files, but **do not** rename the `parse` and `interp` functions.
