---
layout: page
parent: Notes
title: "14. Types"
---

# Types
{: .no_toc }

<details open markdown="block">
  <summary>
    Table of contents
  </summary>
  {: .text-delta }
- TOC
{:toc}
</details>

## Checking Program Invariants Statically

As programs grow larger or more subtle, developers need tools to help them describe and validate statements about program invariants. Invariants, as the name suggests, are statements about program elements that are expected to always hold of those elements. For example, if we write `x : number` in our (to be developed) typed language, we mean that `x` will always hold a number, and that all parts of the program that depend on `x` can rely on this statement being enforced. As we will see, types are just one point in a spectrum of invariants we might wish to state, and static type checking—itself a diverse family of techniques—is also a point in a spectrum of methods we can use to enforce the invariants.

In this module, we will focus especially on _static type checking_: that is, checking (declared) types before the program even executes. You might be familiar with this from your prior use of Haskell or Java. We will explore some of the design space of types and their trade-offs and learn why static typing is an especially powerful and important form of invariant enforcement.

Consider the following program in a typed version of our language. We will define the full language later, but the example should be simple to understand.

```racket
(let ((f (λ (n : int) : int
           (+ n 3))))
  (f #t))
```

Our type system should be able to flag the type error before the program begins execution. The same program (without the type annotations) in ordinary Racket fails only at runtime:

```racket
(let ((f (λ (n)
           (+ n 3))))
  (f #t))
```

But before we define our typed language, we will have to understand what are types.

## Concrete Syntax

The general syntax of the typed language is shown as below. It is same as the language with lambda functions, except for places where new variables are introduced. So lambda function have type annotations attached to each argument and finally another type to denote the type of value it produces. The other place we need types are let bindings.

<!--
\begin{array}{lccl}
\textrm{Values}      & v & ::=  & \textrm{Integer} \mid \texttt{\#t} \mid \texttt{\#f} \mid \texttt{($\lambda$ ($x : T\ \ldots\ x : T$) $: T\ e$)}\\
\textrm{Expressions} & e & ::=  & v \mid x \mid \texttt{(add1 $e$)} \mid \texttt{(sub1 $e$)} \\
                     &   & \mid & \texttt{(+ $e$ $e$)} \mid \texttt{(- $e$ $e$)} \mid \texttt{(* $e$ $e$)} \mid \texttt{(/ $e$ $e$)} \\
                     &   & \mid & \texttt{(zero? $e$)} \mid \texttt{(and $e$ $e$)} \mid \texttt{(<= $e$ $e$)} \mid \texttt{(if $e$ $e$ $e$)} \\
                     &   & \mid & \texttt{(let (($x : T$ $e$)) $e$)} \mid \texttt{($e\ \ldots\ e$)}\\
\end{array}
-->
![Types Grammar]({{site.baseurl}}/images/types-grammar.png)

Before we proceed we still have to describe what types (the `T` in the above figure) are. Types can be base types for constants like `int` and `bool` in our language. They can also be function types denoted by the arrow. The last type in the function type (`T`) is the type of value computed by the function and anything preceeding it are the arguments (`T1` through `Tn`). Thus, `int -> bool` is a function that takes an integer and produces a boolean, while `int -> int -> int` is a function that takes two integer arguments and produces an integer.

<!--
\begin{array}{ccl}
B & ::= & \texttt{int} \mid \texttt{bool}\\
T & ::= & B \mid T \rightarrow T
\end{array}
-->
![Types Grammar]({{site.baseurl}}/images/types-ty-grammar.png)

## Abstract Syntax

To handle these changes in the surface syntax, we have to extend our AST and the parser. The AST is updated such that `Lam` now contains `xts` which is list of argument identifiers and their types and an additional `t` which is the return type of the function. Likewise, the `t` paramater in `Let` stores the type of the identifier being declared.

<!--
\begin{array}{lccl}
\textrm{Expressions} & e & ::=  & \texttt{(Val $v$)} \mid \texttt{(Var $x$)}\mid \texttt{(Lam $xts$ $t$ $e$)} \mid\\
                     &   & \mid & \texttt{(UnOp $u$ $e$)} \mid \texttt{(BinOp $b$ $e$ $e$)} \\
                     &   & \mid & \texttt{(If $e$ $e$ $e$)} \mid \texttt{(Let x t e e)}\\
                     &   & \mid & \texttt{(App e $es$)}\\
\end{array}
-->
![Types AST]({{site.baseurl}}/images/types-ast.png)

To represent these changes we add the new fields to `Lam` and `Let` structs. To represent types, we use the struct `T` to denote base types such as `int` or `bool`. Functions are represented as `FnT` where `args` is the list of arguments types and `ret` is the return type of the function.

```racket
(struct Lam (xts t e)   #:prefab)
(struct Let (x t e1 e2) #:prefab)

(struct T   (t)        #:prefab)
(struct FnT (args ret) #:prefab)
```

Then we updated our parser to pattern match against the `x : T` syntax. The newly defined `parse-type` function converts the surface syntax of the types to our structs. It maps `int` and `bool` to `(T 'int)` and `(T 'bool)`. For function types it recursively parses all the types and creates a `FnT` struct.

```racket
(define (parse s)
  (match s
    [(? integer?)                   (Val s)]
    [(? boolean?)                   (Val s)]
    [(? symbol?)                    (Var s)]
    [(list (? unop? u) e)           (UnOp u (parse e))]
    [(list (? binop? b) e1 e2)      (BinOp b (parse e1) (parse e2))]
    [`(if ,e1 ,e2 ,e3)              (If (parse e1) (parse e2) (parse e3))]
    [`(let ((,x : ,ty ,e1)) ,e2)    (Let x (parse-type ty) (parse e1) (parse e2))]
    [`(lambda (,@xts) : ,t ,e)      (Lam (parse-xts xts) (parse-type t) (parse e))]
    [`(λ      (,@xts) : ,t ,e)      (Lam (parse-xts xts) (parse-type t) (parse e))]
    [(cons e es)                    (App (parse e) (map parse es))]
    [_                              (error "Parse error!")]))

(define (parse-xts xts)
  (match xts
    ['()               '()]
    [`(,x : ,t)        (cons (list x (parse-type t)) '())]
    [`(,x : ,t ,@rest) (cons (list x (parse-type t)) (parse-xts rest))]))

(define (parse-type t)
  (match t
    ['int            (T 'int)]
    ['bool           (T 'bool)]
    [`(-> ,@ts ,ret) (FnT (map parse-type ts) (parse-type ret))]
    [_               (error "Unhandled type")]))
```

Now that we’ve fixed both the term and type structure of the language, let’s make sure we agree on what constitute type errors in our language (and, by fiat, everything not a type error must pass the type checker). Here are a few obvious forms of type errors:

* One or both arguments of `+` is not a number, i.e., is not a `(T 'int)`.
* One or both arguments of `*` is not a number.
* The expression in the function position of an application is not a function, i.e., is not a `FnT`.
* The expression in the function position of an application is a function but the type of the actual argument does not match the type of the formal argument expected by the function.

A natural starting signature for the type-checker would be that it is a procedure that consumes an expression and returns a type for it. If it cannot, it will throw a type error. Because we know expressions contain identifiers, it soon becomes clear that we will want a type environment, which maps names to types, analogous to the value environment we have seen so far.

## Type checker

Just like our interpreter, the type checker pattern matches on the AST and type checks each AST node.

```racket
(define (tc TE e)
  (match e
    [(Val v)         (tc-val v)]
    [(Var x)         (lookup TE x)]
    [(UnOp u e)      (tc-unop TE u e)]
    [(BinOp b e1 e2) (tc-binop TE b e1 e2)]
    [(Let x t e1 e2) (tc-let TE x t e1 e2)]
    [(Lam xts t e)   (tc-lam TE xts t e)]
    [(App e es)      (tc-app TE e es)]))
```

For values, our type checker see if the constant is an integer or boolean returns the corresponding type.

<!--
\frac{v \in \text{Integers}}{\Gamma \vdash \texttt{(Val $v$)} : Int} \quad [\textsc{T-Int}]
\qquad \qquad
\frac{v \in \text{Booleans}}{\Gamma \vdash \texttt{(Val $v$)} : Bool} \quad [\textsc{T-Bool}]
-->
![TC Val]({{site.baseurl}}/images/types-tc-val.png)

The judgment here says in the type enviroment `Γ`, the expression `(Val v)` has the type `Int` if `v` is an integer and `boolean` if `v` is a boolean. In our implementation we use `TE` to denote the type environment. The type of an expression always follows the `:`. Written in code this looks as below:

```racket
(define (tc-val v)
  (match v
    [(? integer?) (T 'int)]
    [(? boolean?) (T 'bool)]
    [_            (error "Unexpected value")]))
```

For unary operations like `add1` and `sub1`, an integer is produced only if the argument to these is an integer. Similarly, `zero?` produces a `bool` only if it's argument is an `int`.

<!--
\frac{\Gamma \vdash e : Int}{\Gamma \vdash \texttt{(UnOp add1 $e$)} : Int} \quad [\textsc{T-Add1}]
\qquad \qquad
\frac{\Gamma \vdash e : Int}{\Gamma \vdash \texttt{(UnOp zero? $e$)} : Bool} \quad [\textsc{T-Zero}]
-->
![TC UnOp]({{site.baseurl}}/images/types-tc-unop.png)

Just like our interpreter, any type derivation in the premise becomes a recursive call. Thus for unary operation, our type checker looks like this:

```racket
(define (tc-unop TE u e)
  (match* (u (tc TE e))
    [('add1  (T 'int)) (T 'int)]
    [('sub1  (T 'int)) (T 'int)]
    [('zero? (T 'int)) (T 'bool)]
    [(_      _)        (error "Type error!")]))
```

<!--
\frac{\Gamma \vdash e_1 : Int \quad \Gamma \vdash e_2 : Int}{\Gamma \vdash \texttt{(BinOp + $e_1$ $e_2$)} : Int} \quad [\textsc{T-Add}]
\qquad \qquad
\frac{\Gamma \vdash e_1 : Bool \quad \Gamma \vdash e_2 : Bool}{\Gamma \vdash \texttt{(BinOp and $e_1$ $e_2$)} : Bool} \quad [\textsc{T-And}]
-->
![TC BinOp]({{site.baseurl}}/images/types-tc-binop.png)

```racket
(define (tc-binop TE b e1 e2)
  (match* (b (tc TE e1) (tc TE e2))
    [('+   (T 'int)  (T 'int))  (T 'int)]
    [('-   (T 'int)  (T 'int))  (T 'int)]
    [('*   (T 'int)  (T 'int))  (T 'int)]
    [('/   (T 'int)  (T 'int))  (T 'int)]
    [('<=  (T 'int)  (T 'int))  (T 'bool)]
    [('and (T 'bool) (T 'bool)) (T 'bool)]
    [(_    _         _)         (error "Type error!")]))
```
<!--
\frac{\Gamma \vdash e_1 : t \quad \Gamma[x \mapsto t] \vdash e_2 : T}{\Gamma \vdash \texttt{(Let $x$ $t$ $e_1$ $e_2$)} : T} \quad [\textsc{T-Let}]
-->
![TC Let]({{site.baseurl}}/images/types-tc-let.png)

```racket
(define (tc-let TE x t e1 e2)
  (if (equal? (tc TE e1) t)
      (tc (store TE x t) e2)
      (error "Type error!")))
```

<!--
\frac{\Gamma[x_1 \mapsto T_1] \vdash e : T}{\Gamma \vdash \texttt{(Lam (($x_1$ $T_1$)) $T$ $e$)} : T_1 \rightarrow T} \quad [\textsc{T-Lam}]
-->
![TC Lam]({{site.baseurl}}/images/types-tc-lam.png)

```racket
(define (tc-lam TE xts t e)
  (if (equal? (tc (append xts TE) e) t)
      (FnT (map last xts) t)
      (error "Type error!")))
```

<!--
\frac{\Gamma \vdash e_1 : T_1 \rightarrow T \qquad \Gamma \vdash e_2 : T_1}{\Gamma \vdash \texttt{(App $e_1$ $e_2$)} : T} \quad [\textsc{T-App}]
-->
![TC App]({{site.baseurl}}/images/types-tc-app.png)

```racket
(define (tc-app TE e es)
  (match (tc TE e)
    [(FnT args ret) (if (equal? args (map (λ (e) (tc TE e)) es))
                        ret
                        (error "Type error!"))]
    [_              (error "Type error!")]))
```

## Type checking conditionals

## Recursion

## Types and runtime

## Type soundness

## Parameterized Types

## Type Inference

---

_These notes are adapted from [CS173 at Brown](https://cs.brown.edu/courses/cs173/2012/book/types.html)._
