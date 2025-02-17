#lang racket

(require rackunit)

(provide interp)
 
;; Expr -> Integer
;; Interpret given expression
(define (interp e)
  (match e
    [(? integer?)      e]
    [(? boolean?)      e]
    [`(add1 ,e)        (+ (interp e)  1)]
    [`(sub1 ,e)        (- (interp e)  1)]
    [`(+ ,e1 ,e2)      (+ (interp e1) (interp e2))]
    [`(- ,e1 ,e2)      (- (interp e1) (interp e2))]
    [`(* ,e1 ,e2)      (* (interp e1) (interp e2))]
    [`(/ ,e1 ,e2)      (/ (interp e1) (interp e2))]
    [`(zero? ,e)       (interp-zero? e)]
    [`(and ,e1 ,e2)    (interp-and e1 e2)]
    [`(<= ,e1 ,e2)     (<= (interp e1) (interp e2))]
    [`(if ,e1 ,e2 ,e3) (interp-if e1 e2 e3)]))

(define (interp-zero? e)
  (match (interp e)
    [0 #t]
    [_ #f]))

(define (interp-and e1 e2)
  (match (interp e1)
    [#f #f]
    [_  (interp e2)]))

(define (interp-if e1 e2 e3)
  (match (interp e1)
    [#f (interp e3)]
    [_  (interp e2)]))
