#lang racket

(require redex rackunit)

(provide L free? α β render-free? render-α render-β render-cbn-rules-judgment render-cbv-rules-judgment) 

(define (not-equal? v1 v2) (not (equal? v1 v2)))

(define-language L
  (x ::= variable-not-otherwise-mentioned)
  (v ::= x (λ (x) e))
  (e ::= v (e e))
  #:binding-forms
  (λ (x) e #:refers-to x))

(module+ test
  (test-match L e (term x))
  (test-match L e (term x))
  (test-match L e (term (λ (x) x)))
  (test-match L e (term (λ (x) y)))
  (test-match L e (term ((λ (x) y) z))))

(define-metafunction L
  α : e x x -> e
  [(α x_1         x_2 x_3) x_3
                                      (side-condition (equal? (term x_1) (term x_2)))]
  [(α x_1         x_2 x_3) x_1
                                      (side-condition (not-equal? (term x_1) (term x_2)))]
  [(α (λ (x_1) e) x_2 x_3) (λ (x_1) e)
                                      (side-condition (equal? (term x_1) (term x_2)))]
  [(α (λ (x_1) e) x_2 x_3) (λ (x_1) (α e x_2 x_3))
                                      (side-condition (not-equal? (term x_1) (term x_2)))]
  [(α (e_1 e_2)   x_2 x_3) ((α e_1 x_2 x_3) (α e_2 x_2 x_3))])

(define-metafunction L
  in : (x ...) x -> boolean
  [(in () _) #f]
  [(in (x_1 x_2 ...) x) #t
                        (side-condition (equal? (term x_1) (term x)))]
  [(in (x_1 x_2 ...) x) (in (x_2 ...) x)
                        (side-condition (not-equal? (term x_1) (term x)))])

(define-metafunction L
  not-in : x (x ...) -> boolean
  [(not-in x ())        ,(not (term (in () x)))]
  [(not-in x_1 (x ...)) ,(not (term (in (x ...) x_1)))])

(define-metafunction L
  free? : (x ...) x e -> boolean
  [(free? ()      x_1 x_2)           ,(equal? (term x_1) (term x_2))]
  [(free? (x ...) x_1 x_2)           ,(and (equal? (term x_1) (term x_2)) (term (not-in x_1 (x ...))))]
  [(free? ()      x_1 (λ (x_2) e_1)) ,(term (free? (x_2)       x_1 e_1))]
  [(free? (x ...) x_1 (λ (x_2) e_1)) ,(term (free? (x_2 x ...) x_1 e_1))]
  [(free? ()      x_1 (e_1 e_2))     ,(or (term (free? () x_1 e_1)) (term (free? () x_1 e_2)))]
  [(free? (x ...) x_1 (e_1 e_2))     ,(or (term (free? (x ...) x_1 e_1)) (term (free? (x ...) x_1 e_2)))])

(define-metafunction L
  β : e x e -> e
  [(β x_1 x_2 e)             e
                                       (side-condition (equal? (term x_1) (term x_2)))]
  [(β x_1 x_2 e)             x_1
                                       (side-condition (not-equal? (term x_1) (term x_2)))]
  [(β (λ (x_1) e_1) x_2 e_2) (λ (x_1) e_1)
                                       (side-condition (equal? (term x_1) (term x_2)))]
  [(β (λ (x_1) e_1) x_2 e_2) (λ (x_1) (β e_1 x_2 e_2))
                                       (side-condition (and (not-equal? (term x_1) (term x_2)) (not (term (free? () x_1 e_2)))))]
  [(β (λ (x_1) e_1) x_2 e_2) (λ (x_t) (β (α e_1 x_1 x_t) x_2 e_2))
                                       (side-condition (and (not-equal? (term x_1) (term x_2)) (term (free? () x_1 e_2))))]
  [(β (e_1 e_2)     x_1 e_3) ((β e_1 x_1 e_3) (β e_2 x_1 e_3))])

(define-judgment-form L
  #:mode (eval I O)
  #:contract (eval e v)

  [(eval e_1 (λ (x) e_3))
   (eval (β e_3 x e_2) v)
   ---------------------- "app-n"
   (eval (e_1 e_2) v)]

  [(eval e_1 (λ (x) e_3))
   (eval e_2 v_2)
   (eval (β e_3 x v_2) v)
   ---------------------- "app-v"
   (eval (e_1 e_2) v)])

(define (renderer e)
  (with-compound-rewriters
      (['+          (λ (lws) (list "(+ "  (list-ref lws 2) " "   (list-ref lws 3) ")"))]
       ['-          (λ (lws) (list "(- "  (list-ref lws 2) " "   (list-ref lws 3) ")"))]
       ['*          (λ (lws) (list "(* "  (list-ref lws 2) " "   (list-ref lws 3) ")"))]
       ['/          (λ (lws) (list "(/ "  (list-ref lws 2) " "   (list-ref lws 3) ")"))]
       ['<=         (λ (lws) (list "(<= " (list-ref lws 2) " "   (list-ref lws 3) ")"))]
       ['eval       (λ (lws) (list ""     (list-ref lws 2) " ⇓ " (list-ref lws 3) "" ))]
       ['not-equal? (λ (lws) (list ""     (list-ref lws 2) " ≠ " (list-ref lws 3) "" ))]
       ['equal?     (λ (lws) (list ""     (list-ref lws 2) " = " (list-ref lws 3) "" ))]
       ['or         (λ (lws) (list ""     (list-ref lws 2) " ∨ " (list-ref lws 3) "" ))]
       ['and        (λ (lws) (list ""     (list-ref lws 2) " ∧ " (list-ref lws 3) "" ))]
       ['not        (λ (lws) (list ""     " ¬ "            (list-ref lws 2) "" ))]
       ['not-in     (λ (lws) (list ""     (list-ref lws 2) " ∉ " (list-ref lws 3) "" ))])
    (e)))

(define (render-free?)
  (renderer (λ () (parameterize ([metafunction-pict-style 'left-right/beside-side-conditions])
                    (render-metafunction free?)))))

(define (render-α)
  (renderer (λ () (parameterize ([metafunction-pict-style 'left-right/beside-side-conditions])
                    (render-metafunction α)))))

(define (render-β)
  (renderer (λ () (parameterize ([metafunction-pict-style 'left-right/beside-side-conditions])
                    (render-metafunction β)))))

(define (render-cbn-rules-judgment)
  (renderer (λ () (parameterize ([judgment-form-cases '("app-n")])
                    (render-judgment-form eval)))))

(define (render-cbv-rules-judgment)
  (renderer (λ () (parameterize ([judgment-form-cases '("app-v")])
                    (render-judgment-form eval)))))

(module+ test
  (test-equal (alpha-equivalent? L (term (α (λ (y) x) x z))
                                   (term (λ (y) z)))
              #t)
  (test-equal (alpha-equivalent? L (term (α (λ (y) x) y z))
                                   (term (λ (y) x)))
              #t)
  (test-equal (term (free? () x (x ((λ (x) x) y))))
              #t)
  (test-equal (term (free? (x) x (x ((λ (x) x) y))))
              #f)
  (test-equal (term (free? () x (z ((λ (x) x) y))))
              #f)
  (test-equal (alpha-equivalent? L (term (β (x x) x (λ (x) (x x))))
                                   (term ((λ (x) (x x)) (λ (x) (x x)))))
              #t)
  (test-equal (alpha-equivalent? L (term (β (λ (y) x) x y))
                                   (term (λ (a) y)))
              #t))
