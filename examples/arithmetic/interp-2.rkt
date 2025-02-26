#lang racket

(require rackunit)

(provide interp)
 
;; Expr -> Integer
;; Interpret given expression
(define (interp e)
  (match e
    [(? integer?) e]
    [`(add1 ,e)   (+ (interp e)  1)]
    [`(sub1 ,e)   (- (interp e)  1)]
    [`(+ ,e1 ,e2) (+ (interp e1) (interp e2))]
    [`(- ,e1 ,e2) (- (interp e1) (interp e2))]
    [`(* ,e1 ,e2) (* (interp e1) (interp e2))]
    [`(/ ,e1 ,e2) (quotient (interp e1) (interp e2))]))
