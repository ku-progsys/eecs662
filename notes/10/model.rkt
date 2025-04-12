#lang racket

(require redex rackunit)

(provide L render-store render-lookup render-eval-rules-judgment-1 render-eval-rules-judgment-2)

(define (not-equal? v1 v2) (not (equal? v1 v2)))

(define-language L
  (E ::= ((x v) ...))
  (x ::= variable-not-otherwise-mentioned)
  (v ::= integer boolean (Closure E e))
  (e ::= v x (λ (x) e) (add1 e) (sub1 e)
     (+ e e) (- e e) (* e e) (/ e e)
     (zero? e) (<= e e) (and e e)
     (if e e e)
     (let ((x e)) e)
     (e e)))

(define-metafunction L
  store : E x v -> E
  [(store ((x_1 v_1) ...) x v) ((x v) (x_1 v_1) ...)])

(define-metafunction L
  lookup : E x -> v
  [(lookup ((x_1 v_1) (x_2 v_2) ...) x_1) v_1]
  [(lookup ((x_1 v_1) (x_2 v_2) ...) x_3) (lookup ((x_2 v_2) ...) x_3)])

(define-judgment-form L
  #:mode (eval I I O)
  #:contract (eval E e v)

  [---------------------- "val"
   (eval E v v)]

  [---------------------- "var"
   (eval E x (lookup E x))]

  [---------------------- "lambda"
   (eval E (λ (x) e) (Closure E (λ (x) e)))]

  [(eval E e v_1)
   ----------------------------- "add1"
   (eval E (add1 e) ,(+ (term v_1) (term 1)))]

  [(eval E e v_1)
   ----------------------------- "sub1"
   (eval E (sub1 e) ,(- (term v_1) (term 1)))]

  [(eval E e_1 v_1)
   (eval E e_2 v_2)
   ------------------------------- "add"
   (eval E (+ e_1 e_2) ,(+ (term v_1) (term v_2)))]

  [(eval E e_1 v_1)
   (eval E e_2 v_2)
   -------------------------------------- "sub"
   (eval E (- e_1 e_2) ,(- (term v_1) (term v_2)))]

  [(eval E e_1 v_1)
   (eval E e_2 v_2)
   -------------------------------------- "mult"
   (eval E (* e_1 e_2) ,(* (term v_1) (term v_2)))]

  [(eval E e_1 v_1)
   (eval E e_2 v_2)
   (side-condition (not-equal? v_2 0))
   -------------------------------------- "div"
   (eval E (/ e_1 e_2) ,(quotient (term v_1) (term v_2)))]

  [(eval E e 0)
   -------------------------------------- "zero-t"
   (eval E (zero? e) #t)]

  [(eval E e v)
   (side-condition (not-equal? v 0))
   -------------------------------------- "zero-f"
   (eval E (zero? e) #f)]

  [(eval E e_1 v_1)
   (eval E e_2 v_2)
   -------------------------------------- "leq"
   (eval E (<= e_1 e_2) ,(<= (term v_1) (term v_2)))]

  [(eval E e_1 #f)
   -------------------------------------- "and-1"
   (eval E (and e_1 e_2) #f)]

  [(eval E e_1 v_1)
   (side-condition (not-equal? v_1 #f))
   (eval E e_2 v_2)
   -------------------------------------- "and-2"
   (eval E (and e_1 e_2) v_2)]

  [(eval E e_1 #f)
   (eval E e_3 v_3)
   -------------------------------------- "if-f"
   (eval E (if e_1 e_2 e_3) v_3)]

  [(eval E e_1 v_1)
   (side-condition (not-equal? v_1 #f))
   (eval E e_2 v_2)
   -------------------------------------- "if-t"
   (eval E (if e_1 e_2 e_3) v_2)]

  [(eval E e_1 v_1)
   (eval (store E x v_1) e_2 v_2)
   -------------------------------------- "let"
   (eval E (let ((x e_1)) e_2) v_2)]

  [(eval E e_1 (Closure E_1 (λ (x) e_3)))
   (eval E e_2 v_2)
   (eval (store E_1 x v_2) e_3 v)
   -------------------------------------- "app"
   (eval E (e_1 e_2) v)])

(define (renderer e)
  (with-compound-rewriters
      (['+          (λ (lws) (list "(+ "  (list-ref lws 2) " "   (list-ref lws 3) ")"))]
       ['-          (λ (lws) (list "(- "  (list-ref lws 2) " "   (list-ref lws 3) ")"))]
       ['*          (λ (lws) (list "(* "  (list-ref lws 2) " "   (list-ref lws 3) ")"))]
       ['/          (λ (lws) (list "(/ "  (list-ref lws 2) " "   (list-ref lws 3) ")"))]
       ['quotient   (λ (lws) (list "(/ "  (list-ref lws 2) " "   (list-ref lws 3) ")"))]
       ['<=         (λ (lws) (list "(<= " (list-ref lws 2) " "   (list-ref lws 3) ")"))]
       ['eval       (λ (lws) (list ""     (list-ref lws 2) " ⊢ " (list-ref lws 3) " ⇓ " (list-ref lws 4) "" ))]
       ['not-equal? (λ (lws) (list ""     (list-ref lws 2) " ≠ " (list-ref lws 3) "" ))]
       ['equal?     (λ (lws) (list ""     (list-ref lws 2) " = " (list-ref lws 3) "" ))]
       ['or         (λ (lws) (list ""     (list-ref lws 2) " ∨ " (list-ref lws 3) "" ))]
       ['and        (λ (lws) (list ""     (list-ref lws 2) " ∧ " (list-ref lws 3) "" ))]
       ['not        (λ (lws) (list ""     " ¬ "            (list-ref lws 2) "" ))]
       ['not-in     (λ (lws) (list ""     (list-ref lws 2) " ∉ " (list-ref lws 3) "" ))]
       ;['lookup     (λ (lws) (list ""     (list-ref lws 2) "[" (list-ref lws 3) "]" "" ))]
       ;['store      (λ (lws) (list ""     (list-ref lws 2) "[" (list-ref lws 3) " ↦ " (list-ref lws 4) "]" "" ))]
       )
    (e)))

(define (render-store)
  (renderer (λ () (parameterize ([metafunction-pict-style 'left-right/beside-side-conditions])
                    (render-metafunction store)))))

(define (render-lookup)
  (renderer (λ () (parameterize ([metafunction-pict-style 'left-right/beside-side-conditions])
                    (render-metafunction lookup)))))

(define (render-eval-rules-judgment-1)
  (renderer (λ () (parameterize ([judgment-form-cases '("val"
                                                        "add1"
                                                        "sub1"
                                                        "add"
                                                        "sub"
                                                        "mult"
                                                        "div"
                                                        "zero-t"
                                                        "zero-f"
                                                        "leq"
                                                        "and-1"
                                                        "and-2"
                                                        "if-t"
                                                        "if-f")])
                    (render-judgment-form eval)))))

(define (render-eval-rules-judgment-2)
  (renderer (λ () (parameterize ([judgment-form-cases '("var"
                                                        "lambda"
                                                        "let"
                                                        "app")])
                    (render-judgment-form eval)))))

(module+ test
  (test-judgment-holds (eval () 3 3))
  (test-judgment-holds (eval ((y 5)) y 5))
  (test-judgment-holds (eval ((x 3) (y 5)) x 3))
  (test-judgment-holds (eval ((x 3)) (λ (x) (add1 x)) (Closure ((x 3)) (λ (x) (add1 x)))))
  (test-judgment-holds (eval () (let ((x 5))
             (let ((fn (λ (n) (- x n))))
               (fn 3))) 2))
  (test-judgment-holds (eval () (let ((x 5))
             (let ((fn (λ (n) (- x n))))
               (let ((x 3))
                 (fn 3)))) 2)))
