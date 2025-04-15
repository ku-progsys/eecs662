#lang racket

(provide interp eval)

(struct Closure (E e) #:prefab)

(define (store E x v)
  (cons (cons x v) E))

(define (lookup E x)
  (match E
    ['() (error "variable not found!")]
    [(cons (cons y v) E) (if (eq? x y) v
                             (lookup E x))]))

(define (interp-div E S e1 e2)
  (match* ((only-int (interp E S e1)) (only-int (interp E S e2)))
    [(v1 0)  (error "Division by 0 not allowed!")]
    [(v1 v2) (quotient v1 v2)]))

(define (only-int v)
  (if (integer? v)
      v
      (error "Integer expected!")))

(define (interp-zero? E S e)
  (match (interp E S e)
    [0 #t]
    [_ #f]))

(define (interp-and E S e1 e2)
  (match (interp E S e1)
    [#f #f]
    [_  (interp E S e2)]))

(define (interp-if E S e1 e2 e3)
  (match (interp E S e1)
    [#f (interp E S e3)]
    [_  (interp E S e2)]))

(define (interp-let E S x e1 e2)
  (let* ((v1 (interp E S e1))
         (E2 (store E x v1)))
    (interp E2 S e2)))

(define (interp-seq E S es)
  (match es
    [(cons e '()) (interp E S e)]
    [(cons e es)  (begin
                    (interp E S e)
                    (interp-seq E S es))]))

(define (interp-new E S e)
  (let ((v (interp E S e))
        (l (gensym)))
    (begin
      (hash-set! S l v)
      l)))

(define (interp-deref E S e)
  (let ((l (interp E S e)))
    (hash-ref S l)))

(define (interp-set E S e1 e2)
  (let ((l (interp E S e1))
        (v (interp E S e2)))
    (begin
      (hash-set! S l v)
      v)))

(define (interp-free E S e)
  (let ((l (interp E S e)))
    (begin0
      (hash-remove! S l)
      l)))

; Env -> State -> Expr -> Val)
(define (interp E S e)
  (match e
    [(? integer?)            e]
    [(? boolean?)            e]
    [(? Closure?)            e]
    [(? symbol?)             (lookup E e)]
    [`(λ (,x) ,e1)           (Closure E e)]
    [`(add1 ,e)              (+ (interp E S e) 1)]
    [`(sub1 ,e)              (- (interp E S e) 1)]
    [`(zero? ,e)             (interp-zero? E S e)]
    [`(+ ,e1 ,e2)            (+ (interp E S e1) (interp E S e2))]
    [`(- ,e1 ,e2)            (- (interp E S e1) (interp E S e2))]
    [`(* ,e1 ,e2)            (* (interp E S e1) (interp E S e2))]
    [`(/ ,e1 ,e2)            (interp-div E S e1 e2)]
    [`(<= ,e1 ,e2)           (<= (only-int (interp E S e1)) (only-int (interp E S e2)))]
    [`(and ,e1 ,e2)          (interp-and E S e1 e2)]
    [`(if ,e1 ,e2 ,e3)       (interp-if E S e1 e2 e3)]
    [`(let ((,x ,e1)) ,e2)   (interp-let E S x e1 e2)]
    [`(new ,e)               (interp-new E S e)]
    [`(deref ,e)             (interp-deref E S e)]
    [`(set ,e1 ,e2)          (interp-set E S e1 e2)]
    [`(free ,e)              (interp-free E S e)]
    [`(seq ,@es)             (interp-seq E S es)]
    [`(,e1 ,e2)              (match (interp E S e1)
                               [(Closure E1 `(λ (,x) ,e3)) (interp (store E1 x (interp E S e2)) S e3)]
                               [_            (error "Cannot apply non-function!")])]
    [_                       (error "Parser error!")]))
