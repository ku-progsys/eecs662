#lang racket

(provide interp)

(struct Closure (E e) #:prefab)

(define (store E x v)
  (cons (cons x v) E))

(define (lookup E x)
  (match E
    ['() (error "variable not found!")]
    [(cons (cons y v) E) (if (eq? x y) v
                             (lookup E x))]))

(define (interp-div E e1 e2)
  (match* ((only-int (interp E e1)) (only-int (interp E e2)))
    [(v1 0)  (error "Division by 0 not allowed!")]
    [(v1 v2) (quotient v1 v2)]))

(define (only-int v)
  (if (integer? v)
      v
      (error "Integer expected!")))

(define (interp-zero? E e)
  (match (interp E e)
    [0 #t]
    [_ #f]))

(define (interp-and E e1 e2)
  (match (interp E e1)
    [#f #f]
    [_  (interp E e2)]))

(define (interp-if E e1 e2 e3)
  (match (interp E e1)
    [#f (interp E e3)]
    [_  (interp E e2)]))

(define (interp-let E x e1 e2)
  (let* ((v1 (interp E e1))
         (E2 (store E x v1)))
    (interp E2 e2)))

; Env -> Expr -> Val
(define (interp E e)
  (match e
    [(? integer?)            e]
    [(? boolean?)            e]
    [(? Closure?)            e]
    [(? symbol?)             (lookup E e)]
    [`(λ (,x) ,e1)           (Closure E e)]
    [`(add1 ,e)              (+ (interp E e) 1)]
    [`(sub1 ,e)              (- (interp E e) 1)]
    [`(zero? ,e)             (interp-zero? E e)]
    [`(+ ,e1 ,e2)            (+ (interp E e1) (interp E e2))]
    [`(- ,e1 ,e2)            (- (interp E e1) (interp E e2))]
    [`(* ,e1 ,e2)            (* (interp E e1) (interp E e2))]
    [`(/ ,e1 ,e2)            (interp-div E e1 e2)]
    [`(<= ,e1 ,e2)           (<= (only-int (interp E e1)) (only-int (interp E e2)))]
    [`(and ,e1 ,e2)          (interp-and E e1 e2)]
    [`(if ,e1 ,e2 ,e3)       (interp-if E e1 e2 e3)]
    [`(let ((,x ,e1)) ,e2)   (interp-let E x e1 e2)]
    [`(,e1 ,e2)              (match (interp E e1)
                               [(Closure E1 `(λ (,x) ,e3)) (interp (store E1 x (interp E e2)) e3)]
                               [_            (error "Cannot apply non-function!")])]
    [_                       (error "Parser error!")]))
