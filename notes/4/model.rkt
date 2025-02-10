#lang racket

(require redex rackunit)

(provide L eval renderer render-eval-rules-judgment)

(define (not-equal? v1 v2) (not (equal? v1 v2)))

(define-language L
  (v ::= integer)
  (e ::= v (add1 e) (sub1 e)
     (+ e e) (- e e) (* e e) (/ e e)))

(module+ test
  (test-match L e (term 42))
  (test-match L e (term (add1 5)))
  (test-match L e (term (sub1 3)))
  (test-match L e (term (+ 3 4)))
  (test-match L e (term (- 42 3)))
  (test-match L e (term (* 7 4)))
  (test-match L e (term (/ 9 4)))
  (test-match L e (term (* (add1 -3) (sub1 4)))))

(define-judgment-form L
  #:mode (eval I O)
  #:contract (eval e v)

  [----------- "value"
   (eval v v)]

  [(eval e v_1)
   ----------------------------- "add1"
   (eval (add1 e) ,(+ (term v_1) (term 1)))]

  [(eval e v_1)
   ----------------------------- "sub1"
   (eval (sub1 e) ,(- (term v_1) (term 1)))]

  [(eval e_1 v_1)
   (eval e_2 v_2)
   ------------------------------- "add"
   (eval (+ e_1 e_2) ,(+ (term v_1) (term v_2)))]

  [(eval e_1 v_1)
   (eval e_2 v_2)
   -------------------------------------- "sub"
   (eval (- e_1 e_2) ,(- (term v_1) (term v_2)))]

  [(eval e_1 v_1)
   (eval e_2 v_2)
   -------------------------------------- "mult"
   (eval (* e_1 e_2) ,(* (term v_1) (term v_2)))]

  [(eval e_1 v_1)
   (eval e_2 v_2)
   (side-condition (not-equal? v_2 0))
   -------------------------------------- "div"
   (eval (/ e_1 e_2) ,(/ (term v_1) (term v_2)))])

(define (renderer e)
  (with-compound-rewriters (['+          (λ (lws) (list "(+ " (list-ref lws 2) " "   (list-ref lws 3) ")"))]
                            ['-          (λ (lws) (list "(- " (list-ref lws 2) " "   (list-ref lws 3) ")"))]
                            ['*          (λ (lws) (list "(* " (list-ref lws 2) " "   (list-ref lws 3) ")"))]
                            ['/          (λ (lws) (list "(/ " (list-ref lws 2) " "   (list-ref lws 3) ")"))]
                            ['eval       (λ (lws) (list ""    (list-ref lws 2) " ⇓ " (list-ref lws 3) "" ))]
                            ['not-equal? (λ (lws) (list ""    (list-ref lws 2) " ≠ " (list-ref lws 3) "" ))]
                            ['equal?     (λ (lws) (list ""    (list-ref lws 2) " = " (list-ref lws 3) "" ))])
    (e)))

(define (render-eval-rules-judgment)
  (renderer (λ () (parameterize ([judgment-form-cases '("add1"
                                                        "sub1"
                                                        "add"
                                                        "sub"
                                                        "mult"
                                                        "div")])
                    (render-judgment-form eval)))))

(module+ test
  (test-judgment-holds (eval 42 42))
  (test-judgment-holds (eval (add1 3) 4))
  (test-judgment-holds (eval (sub1 (add1 3)) 3))
  (test-judgment-holds (eval (+ (add1 3) (sub1 5)) 8))
  (test-judgment-holds (eval (- 7 (sub1 5)) 3))
  (test-judgment-holds (eval (* 7 (sub1 5)) 28))
  (test-judgment-holds (eval (/ 8 (sub1 5)) 2)))
