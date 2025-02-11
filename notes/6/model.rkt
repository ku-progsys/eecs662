#lang racket

(require redex rackunit)

(provide L eval renderer render-eval-rules-judgment)

(define (not-equal? v1 v2) (not (equal? v1 v2)))

(define-language L
  (v ::= integer boolean)
  (e ::= v (add1 e) (sub1 e)
     (+ e e) (- e e) (* e e) (/ e e)
     (zero? e) (<= e e) (and e e)
     (if e e e)))

(module+ test
  (test-match L e (term 42))
  (test-match L e (term #t))
  (test-match L e (term #f))
  (test-match L e (term (* (add1 -3) (sub1 4))))
  (test-match L e (term (zero? (add1 3))))
  (test-match L e (term (if (zero? (add1 3)) 42 #f))))

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
   ; (side-condition (not-equal? v_2 0))
   -------------------------------------- "div"
   (eval (/ e_1 e_2) ,(/ (term v_1) (term v_2)))]

  [(eval e 0)
   -------------------------------------- "zero-t"
   (eval (zero? e) #t)]

  [(eval e v)
   (side-condition (not-equal? v 0))
   -------------------------------------- "zero-f"
   (eval (zero? e) #f)]

  [(eval e_1 v_1)
   (eval e_2 v_2)
   -------------------------------------- "leq"
   (eval (<= e_1 e_2) ,(<= (term v_1) (term v_2)))]

  [(eval e_1 #f)
   (eval e_2 v_2)
   -------------------------------------- "and-1"
   (eval (and e_1 e_2) v_2)]

  [(eval e_1 v_1)
   (side-condition (not-equal? v_1 #f))
   -------------------------------------- "and-2"
   (eval (and e_1 e_2) v_1)]

  [(eval e_1 #f)
   (eval e_3 v_3)
   -------------------------------------- "if-f"
   (eval (if e_1 e_2 e_3) v_3)]

  [(eval e_1 v_1)
   (side-condition (not-equal? v_1 #f))
   (eval e_2 v_2)
   -------------------------------------- "if-t"
   (eval (if e_1 e_2 e_3) v_2)])
   

(define (renderer e)
  (with-compound-rewriters (['+          (λ (lws) (list "(+ "  (list-ref lws 2) " "   (list-ref lws 3) ")"))]
                            ['-          (λ (lws) (list "(- "  (list-ref lws 2) " "   (list-ref lws 3) ")"))]
                            ['*          (λ (lws) (list "(* "  (list-ref lws 2) " "   (list-ref lws 3) ")"))]
                            ['/          (λ (lws) (list "(/ "  (list-ref lws 2) " "   (list-ref lws 3) ")"))]
                            ['<=         (λ (lws) (list "(<= " (list-ref lws 2) " "   (list-ref lws 3) ")"))]
                            ['eval       (λ (lws) (list ""     (list-ref lws 2) " ⇓ " (list-ref lws 3) "" ))]
                            ['not-equal? (λ (lws) (list ""     (list-ref lws 2) " ≠ " (list-ref lws 3) "" ))]
                            ['equal?     (λ (lws) (list ""     (list-ref lws 2) " = " (list-ref lws 3) "" ))])
    (e)))

(define (render-eval-rules-judgment)
  (renderer (λ () (parameterize ([judgment-form-cases '("value"
                                                        "zero-t"
                                                        "zero-f"
                                                        "leq"
                                                        "and-1"
                                                        "and-2"
                                                        "if-f"
                                                        "if-t")])
                    (render-judgment-form eval)))))

(module+ test
  (test-judgment-holds (eval #t #t))
  (test-judgment-holds (eval (zero? 3) #f))
  (test-judgment-holds (eval (zero? (sub1 1)) #t))
  (test-judgment-holds (eval (if (zero? (sub1 1)) (+ 3 4) #f) 7))
  (test-judgment-holds (eval (if (zero? 4) #t 5) 5)))
