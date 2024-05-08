#lang racket

(require redex rackunit)

(provide L eval renderer render-eval-rules-judgment)

(define-language L
  (e  ::= (Val v) (Var x) (Lam (x ...) e) (UnOp u e) (BinOp b e e) (If e e e) (Let x e e) (App e e))
  (v  ::= integer boolean)
  (i  ::= integer)
  (tf ::= boolean)
  (x  ::= variable)
  (u  ::= 'add1 'sub1 'zero?)
  (b  ::= '+ '- '* '/ 'and)
  (r  ::= ((x v) ...)))

(module+ test
  (test-match L e (term (Val 5)))
  (test-match L e (term (Val #t)))
  (test-match L e (term (Val #f)))
  (test-match L e (term (Var y)))
  (test-match L e (term (Lam (x y) (Val 4))))
  (test-match L e (term (UnOp 'add1 (Val 4))))
  (test-match L e (term (BinOp '+ (Val 4) (Val 2))))
  (test-match L e (term (Let x (Val 4) (UnOp 'add1 (Val 2)))))
  (test-match L e (term (App (Val 4) (Val 2)))))

(define-judgment-form L
  #:mode (eval I I O)
  #:contract (eval r e v)
  [----------------- "val"
   (eval r (Val v) v)]

  [(where v (lookup r x))
   ----------------- "var"
   (eval r (Var x) v)]

  [(eval r e i_1) (where i_2 ,(+ (term i_1) (term 1)))
   ------------------------------------------ "add1"
   (eval r (UnOp 'add1 e) i_2)]

  [(eval r e i_1) (where i_2 ,(- (term i_1) (term 1)))
   ------------------------------------------ "sub1"
   (eval r (UnOp 'sub1 e) i_2)]

  [(eval r e v_1) (side-condition (different v_1 0))
   ------------------------------------------------------ "zero?-f"
   (eval r (UnOp 'zero? e) #f)]

  [(eval r e v_1) (side-condition (same v_1 0))
   ------------------------------------------------------ "zero?-t"
   (eval r (UnOp 'zero? e) #t)]

  [(eval r e_1 #f) (eval r e_3 v_3)
   ------------------------------------------------------ "if-f"
   (eval r (If e_1 e_2 e_3) v_3)]

  [(eval r e_1 v_1) (eval r e_2 v_2) (side-condition (different v_1 #t))
   ------------------------------------------------------ "if-t"
   (eval r (If e_1 e_2 e_3) v_2)]

  [(eval r_1 e_1 v_1) (eval (ext r_1 x v_1) e_2 v_2)
   ------------------------------------------------------ "let"
   (eval r_1 (Let x e_1 e_2) v_2)])

(define-metafunction L
  different : v v -> tf
  [(different v_1 v_1) #f]
  [(different v_1 v_2) #t])

(define-metafunction L
  same : v v -> tf
  [(same v_1 v_1) #t]
  [(same v_1 v_2) #f])

(define-metafunction L
  lookup : r x -> v
  [(lookup ((x v) (x_1 v_1) ...)     x) v]
  [(lookup ((x_0 v_0) (x_1 v_1) ...) x) (lookup ((x_1 v_1) ...) x)])


(define-metafunction L
  ext : r x v -> r
  [(ext ((x_0 v_0) ...) x v) ((x v) (x_0 v_0) ...)])

(define (renderer e)
  (with-compound-rewriters (['+         (λ (lws) (list "" (list-ref lws 2) " + " (list-ref lws 3) ""))]
                            ['-         (λ (lws) (list "" (list-ref lws 2) " - " (list-ref lws 3) ""))]
                            ['different (λ (lws) (list "" (list-ref lws 2) " ≠ " (list-ref lws 3) ""))]
                            ['same      (λ (lws) (list "" (list-ref lws 2) " = " (list-ref lws 3) ""))]
                            ['eval      (λ (lws) (list "" (list-ref lws 2) " ⊢ " (list-ref lws 3) " ⇓ " (list-ref lws 4) ""))]
                            ['lookup    (λ (lws) (list "" (list-ref lws 2) "[" (list-ref lws 3) "]" ""))]
                            ['ext       (λ (lws) (list "" (list-ref lws 2) "[" (list-ref lws 3) " ↦ " (list-ref lws 4) "]" ""))])
    (e)))

(define (render-eval-rules-judgment)
  (renderer (λ () (render-judgment-form eval))))

(module+ test
  (test-judgment-holds (eval () (UnOp 'add1  (Val 3)) 4))
  (test-judgment-holds (eval () (UnOp 'sub1  (Val 4)) 3))
  (test-judgment-holds (eval () (UnOp 'zero? (Val 4)) #f))
  (test-judgment-holds (eval () (UnOp 'zero? (UnOp 'sub1 (Val 1))) #t))
  (test-judgment-holds (eval ((x 42)) (Var x) 42))
  (test-judgment-holds (eval () (Let y (UnOp 'add1 (Val 3)) (Var y)) 4)))