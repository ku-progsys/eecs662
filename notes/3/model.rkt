#lang racket

(require redex rackunit)

(provide L eval renderer render-eval-rules-judgment)

(define-language L
  (v ::= integer)
  (e ::= v))

(module+ test
  (test-match L e (term 5))
  (test-match L e (term -1))
  (test-match L e (term 42)))

(define-judgment-form L
  #:mode (eval I O)
  #:contract (eval e v)

  [------------------------- "value"
   (eval v v)])

(define (renderer e)
  (with-compound-rewriters (['eval (λ (lws) (list "" (list-ref lws 2) " ⇓ " (list-ref lws 3) ""))])
    (e)))

(define (render-eval-rules-judgment)
  (renderer (λ () (render-judgment-form eval))))

(module+ test
  (test-judgment-holds (eval 3 3))
  (test-judgment-holds (eval -1 -1))
  (test-judgment-holds (eval 42 42)))
