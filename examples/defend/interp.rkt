#lang racket

(require rackunit)

(provide interp)
 
;; Expr -> Value
;; Interpret given expression
(define (interp e)
  (match e
    [(? integer?)      e]
    [(? boolean?)      e]
    [`(add1 ,e)        (+ (only-int (interp e))  1)]
    [`(sub1 ,e)        (- (only-int (interp e))  1)]
    [`(+ ,e1 ,e2)      (+ (only-int (interp e1)) (only-int (interp e2)))]
    [`(- ,e1 ,e2)      (- (only-int (interp e1)) (only-int (interp e2)))]
    [`(* ,e1 ,e2)      (* (only-int (interp e1)) (only-int (interp e2)))]
    [`(/ ,e1 ,e2)      (interp-div e1 e2)]
    [`(zero? ,e)       (interp-zero? e)]
    [`(and ,e1 ,e2)    (interp-and e1 e2)]
    [`(<= ,e1 ,e2)     (<= (only-int (interp e1)) (only-int (interp e2)))]
    [`(if ,e1 ,e2 ,e3) (interp-if e1 e2 e3)]
    [_                 (error "Parser error!")]))

(define (interp-div e1 e2)
  (match* ((only-int (interp e1)) (only-int (interp e2)))
    [(v1 0)  (error "Division by 0 not allowed!")]
    [(v1 v2) (quotient v1 v2)]))

(define (only-int v)
  (if (integer? v)
      v
      (error "Integer expected!")))

(define (interp-zero? e)
  (match (interp e)
    [0 #t]
    [_ #f]))

(define (interp-and e1 e2)
  (match (interp e1)
    [#f #f]
    [_  (interp e2)]))

(define (interp-if e1 e2 e3)
  (match (interp e1)
    [#f (interp e3)]
    [_  (interp e2)]))
