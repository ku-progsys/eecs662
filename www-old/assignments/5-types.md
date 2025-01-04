---
layout: page
parent: Assignments
title: 5. Type System
---

# Type System

You are given a zip file on Canvas `hw5.zip` with a starter code for the assignment.

The goal of this assignment is to extend the type system we discussed in class to the language with state. With this change, you will extend your type system to go from a functional language to an imperative one.

More concretely, the language implemented in the starter code has the sequencing form `begin`, ways to allocate, deallocate, and update state via `new`, `deref`, and `set!` respectively just like the language from the [mutation and state]({{site.baseurl}}/notes/13-mutation-state/) module. Additionaly, the type system discussed in class has also been retrofitted onto this but not completely adapted to support state. Your goal in this assignment is add support for references in the type system.

References in this type system is denoted by `ref`. If you parse this through the `parse-type` function this is what you get:

```racket
> (parse-type 'ref)
'#s(T ref)
```

However, just denoting references with a base type `ref` does not tell was anything about the underlying data type that the reference points to. For example, if `x` is a reference we have no idea what kind of value `(deref x)` will return. This can cause unsoundness in the type system! To mitigate this we have to track what type of values references point to. To use this we will use parametric polymorphism where the base type will be `ref`, parametrized by the type of value it is referencing. For example, `(new 5)` will have a type `(ref int)`, `(new #t)` will have a type `(ref bool)`. Derefencing will then give us back the expected type: `(deref (new 5))` will be of type `int`.

These parameterized `ref` types are represented using `ParamT` struct:

```racket
> (parse-type '(ref int))
'#s(ParamT #s(T ref) #s(T int))
```

To add support for this, you have add 4 functions, all of which are left as `TODO` in the provided code.

**Note:** This language is the typed version of our language. Any test programs in this language will require type annotations for variables and function declarations. You only need to change `type.rkt` for this assignment.

### Type `begin`

`begin` is used to define a sequence of expressions in the program. When a sequence of expressions are type checked, all expressions are individually type checked, but the entire sequence has the type of the last expression. Your implementation will go in `tc-seq`.

```racket
; bool type
(begin
  (let ((x : int (+ 5 4)))
    (+ x 2))
  #t)
```

### Type `new`

`new` is used to allocate a value on to the program state. The type of a `(new e)` is the reference to the type of `e`. Your implementation will go in `tc-new`.

```racket
; (ref (-> int int)) type
(new (λ (x : int) : int x))
```

### Type `deref`

`deref` is used to dereference a reference to the program state. The type of a `(deref e)` is the dereferenced type of `e`. It is a type error is `e` is not a reference. Your implementation will go in `tc-deref`.

```racket
; (-> int int) type
(deref (new (λ (x : int) : int x)))
```

### Type `set!`

`set!` is used to mutate value in the program state and returns the updated value. The type of a `(set! e1 e2)` is the type `e2` if `e1` has a reference to type of `e2`. This means once a location has been created in the state with a particular type of value, it can only store values of the same type in future. Your implementation will go in `tc-set!`.

```racket
; int type
(set! (new 4) 5)

; type error
(set! (new 4) #t)
```

## Testing

You should test your code by writing test cases and adding them to relevant files. Use the command raco test [filename] to test your code. Alternatively, pressing “Run” in Dr. Racket will also run your test cases. There are a lot of cases in this assignment, so test your code carefully.

For grading, your submitted interpreter will be tested on multiple inputs that should work. Writing your own test cases will give you confidence that your code can handle previously unseen expressions.

## Submitting

Submit your work on GradeScope. You should submit all Racket files: `ast.rkt`, `parser.rkt`, `interp.rkt`, and `type.rkt`. You may add any auxiliary functions you need to these files, but do not rename the functions you are asked to write.
