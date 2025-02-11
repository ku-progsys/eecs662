#lang racket

(require redex)

(provide L renderer)

(define-language L
  (v ::= variable true false)
  (t ::= v (and t t) (or t t) (not t)
     (==> t t) (<=> t t) (T t t)))


(define (renderer e)
  (with-compound-rewriters (['<=> (λ (lws) (list "" (list-ref lws 2) " ⇔ "   (list-ref lws 3) ""))]
                            ['==> (λ (lws) (list "" (list-ref lws 2) " ⇒ "   (list-ref lws 3) ""))]
                            ['and (λ (lws) (list "" (list-ref lws 2) " ∧ "   (list-ref lws 3) ""))]
                            ['or  (λ (lws) (list "" (list-ref lws 2) " ∨ "   (list-ref lws 3) ""))]
                            ['not (λ (lws) (list "" " ¬ "                    (list-ref lws 2) ""))]
                            ['T   (λ (lws) (list "" (list-ref lws 2) " ⊢ "   (list-ref lws 3) ""))])
    (e)))
