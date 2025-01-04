#lang racket

(provide interp)
 
;; Expr -> Integer
;; Interpret given expression
(define (interp e)
  (match e
    [(? integer?) e]))
