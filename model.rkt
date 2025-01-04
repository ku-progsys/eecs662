#lang racket

(require redex rackunit)

(provide L eval renderer render-eval-rules-judgment)

(define (not-equal? v1 v2) (not (equal? v1 v2)))

(define-language L
  (e ::= v x (add1 e) (sub1 e) (zero? e) (+ e e) (- e e) (* e e) (/ e e) (and e e) (if e e e) (let ((x e)) e) (λ (x) e) (e e))
  (v ::= integer boolean (Closure E x e))
  (x ::= variable-not-otherwise-mentioned)
  (E ::= ((x v) ...)))

(module+ test
  (test-match L e (term 5))
  (test-match L e (term #t))
  (test-match L e (term #f))
  (test-match L e (term y))
  (test-match L e (term (λ (x) 4)))
  (test-match L e (term (add1 4)))
  (test-match L e (term (+ 4 2)))
  (test-match L e (term (let ((x 4)) (add1 2))))
  (test-match L e (term (4 2))))

(define-judgment-form L
  #:mode (eval I I O)
  #:contract (eval E e v)

  [------------------- "val"
   (eval E v v)]

  [----------------------------------- "lam"
   (eval E (λ (x) e) (Closure E x e))]

  [(where v (lookup E x))
   ---------------------- "var"
   (eval E x v)]

  [(eval E e v_1)
   (where v_2 ,(+ (term v_1) (term 1)))
   ------------------------------------ "add1"
   (eval E (add1 e) v_2)]

  [(eval E e v_1)
   (where v_2 ,(- (term v_1) (term 1)))
   ------------------------------------ "sub1"
   (eval E (sub1 e) v_2)]

  [(eval E e v_1)
   (side-condition (not-equal? v_1 0))
   ---------------------------------- "zero?-f"
   (eval E (zero? e) #f)]

  [(eval E e v_1)
   (side-condition (equal? v_1 0))
   ----------------------------- "zero?-t"
   (eval E (zero? e) #t)]

  [(eval E e_1 v_1)
   (eval E e_2 v_2)
   (where v ,(+ (term v_1) (term v_2)))
   ------------------------------------ "+"
   (eval E (+ e_1 e_2) v)]

  [(eval E e_1 #f)
   (eval E e_3 v_3)
   ------------------------------ "if-f"
   (eval E (if e_1 e_2 e_3) v_3)]

  [(eval E e_1 v_1)
   (eval E e_2 v_2)
   (side-condition (equal? v_1 #t))
   ----------------------------------- "if-t"
   (eval E (if e_1 e_2 e_3) v_2)]

  [(eval E e_1 v_1)
   (eval (extend E x v_1) e_2 v_2)
   ------------------------------- "let"
   (eval E (let ((x e_1)) e_2) v_2)]

  [(eval E e_1 (Closure E_1 x e))
   (eval E e_2 v_2)
   (eval (extend E_1 x v_2) e v)
   ------------------------------ "app"
   (eval E (e_1 e_2) v)])

(define-metafunction L
  lookup : E x -> v
  [(lookup ((x v) (x_1 v_1) ...)     x) v]
  [(lookup ((x_0 v_0) (x_1 v_1) ...) x) (lookup ((x_1 v_1) ...) x)])

(define-metafunction L
  extend : E x v -> E
  [(extend ((x_0 v_0) ...) x v) ((x v) (x_0 v_0) ...)])

(define (renderer e)
  (with-compound-rewriters (['+          (λ (lws) (list "" (list-ref lws 2) " + " (list-ref lws 3) ""))]
                            ['-          (λ (lws) (list "" (list-ref lws 2) " - " (list-ref lws 3) ""))]
                            ['not-equal? (λ (lws) (list "" (list-ref lws 2) " ≠ " (list-ref lws 3) ""))]
                            ['equal?     (λ (lws) (list "" (list-ref lws 2) " = " (list-ref lws 3) ""))]
                            ['eval       (λ (lws) (list "" (list-ref lws 2) " ⊢ " (list-ref lws 3) " ⇓ " (list-ref lws 4) ""))]
                            ['lookup     (λ (lws) (list "" (list-ref lws 2) "[" (list-ref lws 3) "]" ""))]
                            ['extend     (λ (lws) (list "" (list-ref lws 2) "[" (list-ref lws 3) " ↦ " (list-ref lws 4) "]" ""))])
    (e)))

(define (render-eval-rules-judgment)
  (renderer (λ () (render-judgment-form eval))))

(module+ test
  (test-judgment-holds (eval () (add1 3) 4))
  (test-judgment-holds (eval () (sub1 4) 3))
  (test-judgment-holds (eval () (zero? 4) #f))
  (test-judgment-holds (eval () (zero? (sub1 1)) #t))
  (test-judgment-holds (eval ((x 42)) x 42))
  (test-judgment-holds (eval () (let ((y (add1 3))) y) 4))
  (test-judgment-holds (eval ((x 1) (y 2)) (+ x y) 3))
  (test-judgment-holds (eval () ((let ((x 3)) (λ (y) (+ y x))) 2) 5)))
