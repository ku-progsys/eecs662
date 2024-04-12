---
layout: page
parent: Notes
title: "12. Programs as Data Structures"
---

# Programs as Data Structures
{: .no_toc }

<details open markdown="block">
  <summary>
    Table of contents
  </summary>
  {: .text-delta }
- TOC
{:toc}
</details>

## Computing over programs

We’ve seen repeatedly that the result of parsing an expression is an AST. ASTs are essentially a tree data structure, but they denote programs. Until now, we have only written interpreters, i.e., programs that consume our AST data structure and produces a value in our language. However, we have no reason to be limited to just an interpreter. We can take this concept further and look at other kinds of programs that manipulates programs.

In general, these take the form of tree walking algorithms. This will be demonstrated by the following examples.

## Queries on programs

Often times, there are programming tools that compute some queries over a given program. As an example, consider a query that computes a list of the integer constants used through a given program. We will define it as a depth-first style tree traversal---except, the nodes in our tree will be the AST nodes. To do this, we need to write a pattern match with cases handling all the AST nodes:

```racket
;; Prog -> Listof Int
(define (find-ints e)
  (match e
    [(Val v)         (if (integer? v)
                         (list v)
                         '())]
    [(Var x)         '()]
    [(UnOp u e)      (find-ints e)]
    [(BinOp b e1 e2) (append (find-ints e1) (find-ints e2))]
    [(If e1 e2 e3)   (append (find-ints e1) (find-ints e2) (find-ints e3))]
    [(Let x e1 e2)   (append (find-ints e1) (find-ints e2))]
    [(Lam xs e)      (find-ints e)]
    [(Defn x xs e)   (find-ints e)]
    [(DefnV x e)     (find-ints e)]
    [(App e es)      (append (find-ints e) (foldl append '() (map find-ints es)))]
    [(Prog ds e)     (append (find-ints e) (foldl append '() (map find-ints ds)))]))
```

Whenever `find-ints` comes across a `Val` node, it checks if the value is an integer and returns the integer as a list. It returns an empty list otherwise. All other cases just compose this result from the `Val` node correctly. For example, a variable cannot ever have an an integer constant, so `Var` always returns empty list. `UnOp`, `BinOp`, `If`, etc. all themselves will not contain integer constants, but their subexpressions may. So these cases recursively calls `find-ints` on the subexpressions. The `App` and `Prog` cases do the same thing, but as they can have variable number of subexpressions, we apply `find-ints` on each of them individually and append all those expressions together.

Running this function on a few examples, we can see it behaves as expected:

```racket
> (find-ints (parse-prog '((let ((x (add1 67)))
                             (/ 6 x)))))
'(67 6)

> (find-ints (parse-prog '((define (foo x y)
                             (+ (+ x 2) y))
                           (add1 (sub1 (if (zero? 6) (foo 4) 5))))))
'(6 4 5 2)
```

If notice the last example carefully, we declared `foo` to take two arguments, but we called `foo` with only one argument. Our `find-ints` query ran perfectly fine! Our `find-ints` worked with the purely with the given program's _syntax_. It did not have access to the interpreter, thus does nopt care about the programs semantics. It did not check if a declared function is being called with the required number of arguments.

## Unparsing

We used a parser to go from a source program to the parsed AST. For example:

```racket
(parse-prog '((define (foo x y)
                  (+ (+ x 2) y))

                (add1 (sub1 (if (zero? 6)
                                (foo 4 7)
                                5)))))
'#s(Prog (#s(Defn foo (x y) #s(BinOp + #s(BinOp + #s(Var x) #s(Val 2)) #s(Var y)))) #s(UnOp add1 #s(UnOp sub1 #s(If #s(UnOp zero? #s(Val 6)) #s(App #s(Var foo) (#s(Val 4) #s(Val 7))) #s(Val 5)))))
```

However, we can also go back to the program source from the AST as well by writing a simple functions:

```racket
;; Expr -> S-expr
(define (unparse e)
  (match e
    [(Val v) v]
    [(Var x) x]
    [(UnOp u e) (list u (unparse e))]
    [(BinOp b e1 e2) (list b (unparse e1) (unparse e2))]
    [(If e1 e2 e3) (list 'if (unparse e1) (unparse e2) (unparse e3))]
    [(Let x e1 e2) (list 'let (list (list x (unparse e1))) (unparse e2))]
    [(Lam xs e) (list 'λ xs (unparse e))]
    [(App ef es) (cons (unparse ef) (map unparse es))]))

;; Defn -> S-expr
(define (unparse-defn e)
  (match e
    [(Defn f xs e) (list 'define (cons f xs) (unparse e))]
    [(DefnV x e) (list 'define x (unparse e))]))

;; Prog -> S-expr
(define (unparse-prog e)
  (match e
    [(Prog ds e) (append (map unparse-defn ds) (unparse e))]))
```

`unparse` converts the `Val` node to the just the value it stores. `Var` is similarly converted to the just the symbol representing the varible. Everything else (`UnOp`, `BinOp`, `Lam`, etc.) that contains subexpressions are composed together into a list with the `unparse`-d subexpression. At it's core, this is reconstructing the s-expression back from the AST. The `unparse-defn` does the same thing for `define` nodes and `unparse-prog` unparses the top-level definitions and the final expression. We can see this by running:

```racket
> (unparse-prog '#s(Prog (#s(Defn foo (x y) #s(BinOp + #s(BinOp + #s(Var x) #s(Val 2)) #s(Var y)))) #s(UnOp add1 #s(UnOp sub1 #s(If #s(UnOp zero? #s(Val 6)) #s(App #s(Var foo) (#s(Val 4) #s(Val 7))) #s(Val 5))))))
'((define (foo x y) (+ (+ x 2) y)) add1 (sub1 (if (zero? 6) (foo 4 7) 5)))
```

We applied `unparse-prog` to the result of parsing the above example program and we can see we get back the same s-expression we had as the original input.

## Program Rewrites

We can also write functions that run over programs and rewrites individual terms to be something else. It turns out to be useful in a variety of scenarios:


### Elaboration

Until now, the recipe we used to design languages broadly fell in these 4 steps:

* Design a syntax for a language feature
* Parse concrete syntax to AST
* Give semantics to the language
* Implement semantics in the interpreter

We repeated this over and over again as we added new features to the language. Many times, however, new language features do not add any new expressive power to the language. In such cases it should be possible to rewrite a term to something we can already express in the language. Let us look at it with an example. We added let bindings first, followed by lambda functions. But, lambda functions allow us to represent let bindings easily:

```racket
(let ((x (add1 5)))
  (+ x 6))
; is equivalent to
((λ (x) (+ x 6)) (add1 5))
```

or more generally

```racket
(let ((x e1)) e2)
; is equivalent to
((λ (x) e2) e1)
```

We could simplify our interpreter by removing all the places we handle let and by just rewriting all let-bindings to be represented using lambdas. This means less code (and bugs) in the interpreter. Our interpreter then represents only a smaller version of the _surface language_, called the _core language_! The process of rewriting a surface syntax to a smaller core language is called _elaboration_. The elaborator usually is simpler than an interpreter and it usually needs to work only about the new feature. The original language need not be verified again. Tested certainly, but because we are not truly adding anything to the language except syntax all we need to worry about is the new definitions and whether they implement the new feature correctly. This strategy is used in Haskell, where the Haskell compiler (GHC) implements only a smaller core language and programs in the Haskell surface syntax are reduced to the core language.

We can do something similar for the let-binding example above:

```racket
;; Prog -> Prog
(define (elaborate e)
  (match e
    [(Val v)         (Val v)]
    [(Var x)         (Var x)]
    [(UnOp u e)      (UnOp u (elaborate e))]
    [(BinOp b e1 e2) (BinOp b (elaborate e1) (elaborate e2))]
    [(If e1 e2 e3)   (If (elaborate e1) (elaborate e2) (elaborate e3))]
    [(Let x e1 e2)   (App (Lam x (elaborate e2)) (list (elaborate e1)))]
    [(Lam xs e)      (Lam xs (elaborate e))]
    [(Defn x xs e)   (Defn x xs (elaborate e))]
    [(DefnV x e)     (DefnV x (elaborate e))]
    [(App e es)      (App (elaborate e) (map elaborate es))]
    [(Prog ds e)     (Prog (map elaborate ds) (elaborate e))]))
```

Notice the change from the previous programs that worked over the AST. Here our function works over a program and produces a _program_. In constrast, the intepreter works over the program and produces a _value_.

Here we are recursively calling `elaborate` on the subexpressions of all AST nodes. The real change is only in the `Let` case, where it is rewritten to a function application to `e1` with the function as a lambda with body as `e2`.

```racket
> (unparse-prog (elaborate (parse-prog '((let ((x (add1 5)))
                                           (+ x 6))))))
'((λ x (+ x 6)) (add1 5))
```

### Optimizations

Most if not all modern compilers and interpreters perform some kind of optimization on programs they process. Such optimizations range from simple function inlining and elimination of constant calculations to sophisticated variable elimination and loop unrolling.

This simple example will take a Lambda program and perform optimizations on numerical calculations. We could write some rules that always hold true for integers:

```
x - x == 0
x - 0 == x
```

These rules are not exhaustive, but it gives you an idea of the kind of rules that can be used for optimization. Let’s define an optimization that replaces each expression on the left with the corresponding expression on the right. These are optimizations because the left-hand side has more computations that are reduced on the right-hand side, which means computing the right hand side expressions will be faster.

```racket
;; Prog -> Prog
(define (optimize e)
  (match e
    [(Val v)         (Val v)]
    [(Var x)         (Var x)]
    [(UnOp u e)      (UnOp u (optimize e))]
    [(BinOp b e1 e2) (optimize-binop b (optimize e1) (optimize e2))]
    [(If e1 e2 e3)   (If (optimize e1) (optimize e2) (optimize e3))]
    [(Let x e1 e2)   (App (Lam x (optimize e2)) (list (optimize e1)))]
    [(Lam xs e)      (Lam xs (optimize e))]
    [(Defn x xs e)   (Defn x xs (optimize e))]
    [(DefnV x e)     (DefnV x (optimize e))]
    [(App e es)      (App (optimize e) (map optimize es))]
    [(Prog ds e)     (Prog (map optimize ds) (optimize e))]))

(define (optimize-binop b e1 e2)
  (match b
    ['- (cond
          ;; e - e == 0
          [(equal? e1 e2)      (Val 0)]
          ;; e - 0 == e
          [(equal? e2 (Val 0)) e1]
          [else                (BinOp b e1 e2)])]
    [_ (BinOp b e1 e2)]))
```

Again, our `optimize` routine is calling itself recursively on the subexpression. Note, that all the optimizations we wrote are for the `BinOp` AST node, so our optimizations are only limited to `BinOp`s. The `BinOp` case calls `optimize-binop`, where we encode these rules. We check for the `-` operation, if both operands are same, the result is a `0`, or if the second operand is `0`, the result is the first operand. Note, our program must always return AST nodes and not the values directly. As we are rewriting programs, we have to take care to ensure we produce a program.

### Substitution

If you go back to let-bindings, we covered [substition]({{site.baseurl}}/notes/08-let-bindings/#substitution) to replace variables to their values. It is essentially the same idea we are using here. As we have discussed substition before, we will not cover it again, but you can connect the dots.
