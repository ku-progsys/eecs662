#lang racket

(provide bool-ops)

(define (bool->str b)
  (match b
    [#t "T"]
    [#f "F"]))

(define (bool-ops op)
   (map (λ (p)
          (map bool->str (list (first p) (second p) (op (first p) (second p)))))
        (cartesian-product '(#t #f) '(#t #f))))