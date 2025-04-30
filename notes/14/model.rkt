#lang racket

(require redex rackunit)

(provide L LUnion LLetrec
         render-store
         render-lookup
         render-types-lang
         render-types-rules-judgment-1
         render-types-rules-judgment-2
         render-types-rules-judgment-3
         render-types-rules-judgment-4)

(define (not-equal? v1 v2) (not (equal? v1 v2)))

(define-language L
  (x ::= variable-not-otherwise-mentioned)
  (v ::= integer boolean (λ (x : T) : T e))
  (e ::= v x (add1 e) (sub1 e)
     (+ e e) (- e e) (* e e) (/ e e)
     (zero? e) (<= e e) (and e e)
     (if e e e)
     (let ((x : T e)) e)
     (e e))
  (T ::= Int Bool (-> T T))
  (Γ ::= · ((x : T) Γ)))

(define-extended-language LUnion
  L
  (T ::= .... (U T T)))

(define-extended-language LLetrec
  L
  (e ::= .... (letrec ((x : T)) e)))

(define-metafunction L
  store : Γ x T -> Γ
  [(store Γ x T) ((x : T) Γ)])

(define-metafunction L
  lookup : Γ x -> T
  [(lookup ((x_1 : T_1) Γ_1) x_1) T_1]
  [(lookup ((x_1 : T_1) Γ_1) x_2) (lookup Γ_1 x_2)])

(define-judgment-form L
  #:mode (types I I O)
  #:contract (types Γ e T)

  [--------------------- "int"
   (types Γ integer Int)]

  [---------------------- "bool"
   (types Γ boolean Bool)]

  [(types (store Γ x T_1) e T_2)
   -------------------------------------------- "lambda"
   (types Γ (λ (x : T_1) : T_2 e) (-> T_1 T_2))]

  [------------------------ "var"
   (types Γ x (lookup Γ x))]

  [(types Γ e Int)
   ---------------------- "add1"
   (types Γ (add1 e) Int)]

  [(types Γ e Int)
   ---------------------- "sub1"
   (types Γ (sub1 e) Int)]

  [(types Γ e_1 Int)
   (types Γ e_2 Int)
   ------------------------- "add"
   (types Γ (+ e_1 e_2) Int)]

  [(types Γ e_1 Int)
   (types Γ e_2 Int)
   ------------------------- "sub"
   (types Γ (- e_1 e_2) Int)]

  [(types Γ e_1 Int)
   (types Γ e_2 Int)
   ------------------------- "mult"
   (types Γ (* e_1 e_2) Int)]

  [(types Γ e_1 Int)
   (types Γ e_2 Int)
   ------------------------- "div"
   (types Γ (/ e_1 e_2) Int)]

  [(types Γ e Int)
   ------------------------ "zero"
   (types Γ (zero? e) Bool)]

  [(types Γ e_1 Int)
   (types Γ e_2 Int)
   --------------------------- "leq"
   (types Γ (<= e_1 e_2) Bool)]

  [(types Γ e_1 T)
   (types Γ e_2 T)
   ------------------------- "and"
   (types Γ (and e_1 e_2) T)]

  [(types Γ e_1 Bool)
   (types Γ e_2 T)
   (types Γ e_3 T)
   ---------------------------- "if"
   (types Γ (if e_1 e_2 e_3) T)]

  [(types Γ e_1 T_1)
   (types (store Γ x T_1) e_2 T_2)
   --------------------------------------- "let"
   (types Γ (let ((x : T_1 e_1)) e_2) T_2)]

  [(types Γ e_1 (-> T_1 T_2))
   (types Γ e_2 T_1)
   ----------------------- "app"
   (types Γ (e_1 e_2) T_2)])

(define-extended-judgment-form LUnion types
  #:mode (typesU I I O)
  #:contract (typesU Γ e T)

  [(typesU Γ e_1 T_1)
   (typesU Γ e_2 T_2)
   (typesU Γ e_3 T_3)
   ---------------------------- "if"
   (typesU Γ (if e_1 e_2 e_3) (U T_2 T_3))])

(define-extended-judgment-form LLetrec types
  #:mode (typesLetrec I I O)
  #:contract (typesLetrec Γ e T)

  [(typesLetrec (store Γ x T_1) e_1 T_1)
   (typesLetrec (store Γ x T_1) e_2 T_2)
   ------------------------------------- "letrec"
   (typesLetrec Γ (letrec ((x : T_1 e_1)) e_2) T_2)])

(define (renderer e)
  (with-compound-rewriters
      (['+          (λ (lws) (list "(+ "  (list-ref lws 2) " "   (list-ref lws 3) ")"))]
       ['-          (λ (lws) (list "(- "  (list-ref lws 2) " "   (list-ref lws 3) ")"))]
       ['*          (λ (lws) (list "(* "  (list-ref lws 2) " "   (list-ref lws 3) ")"))]
       ['/          (λ (lws) (list "(/ "  (list-ref lws 2) " "   (list-ref lws 3) ")"))]
       ['<=         (λ (lws) (list "(<= " (list-ref lws 2) " "   (list-ref lws 3) ")"))]
       ['U          (λ (lws) (list "("  (list-ref lws 2) " U "   (list-ref lws 3) ")"))]
       ['types      (λ (lws) (list ""     (list-ref lws 2) " ⊢ " (list-ref lws 3) " : " (list-ref lws 4) "" ))]
       ['typesU     (λ (lws) (list ""     (list-ref lws 2) " ⊢ " (list-ref lws 3) " : " (list-ref lws 4) "" ))]
       ['typesLetrec (λ (lws) (list ""     (list-ref lws 2) " ⊢ " (list-ref lws 3) " : " (list-ref lws 4) "" ))]
       ['->         (λ (lws) (list "("    (list-ref lws 2) " → " (list-ref lws 3) ")" ))]
       ['lookup     (λ (lws) (list ""     (list-ref lws 2) "[" (list-ref lws 3) "]" "" ))]
       ['store      (λ (lws) (list ""     (list-ref lws 2) "[" (list-ref lws 3) " ↦ " (list-ref lws 4) "]" "" ))])
    (e)))

(define (render-store)
  (renderer (λ () (parameterize ([metafunction-pict-style 'left-right/beside-side-conditions])
                    (render-metafunction store)))))

(define (render-lookup)
  (renderer (λ () (parameterize ([metafunction-pict-style 'left-right/beside-side-conditions])
                    (render-metafunction lookup)))))

(define (render-types-rules-judgment-1)
  (renderer (λ () (parameterize ([judgment-form-cases '("int"
                                                        "bool"
                                                        "add1"
                                                        "sub1"
                                                        "add"
                                                        "sub"
                                                        "mult"
                                                        "div"
                                                        "zero"
                                                        "leq"
                                                        "and"
                                                        "if")])
                    (render-judgment-form types)))))

(define (render-types-rules-judgment-2)
  (renderer (λ () (parameterize ([judgment-form-cases '("var"
                                                        "lambda"
                                                        "let"
                                                        "app")])
                    (render-judgment-form types)))))

(define (render-types-rules-judgment-3)
  (renderer (λ () (parameterize ([judgment-form-cases '("if")])
                    (render-judgment-form typesU)))))

(define (render-types-rules-judgment-4)
  (renderer (λ () (parameterize ([judgment-form-cases '("letrec")])
                    (render-judgment-form typesLetrec)))))

(define (render-types-lang)
  (renderer (λ () (parameterize ([metafunction-pict-style 'left-right/beside-side-conditions])
                    (render-language L)))))

(module+ test
  (test-judgment-holds (types · 3 Int))
  (test-judgment-holds (types ((x : Int) ((y : Bool) ·)) y Bool))
  (test-judgment-holds (types ((x : Int) ((y : Bool) ·)) x Int))
  (test-judgment-holds (types ((x : Bool) ·) (λ (x : Int) (add1 x) : Int) (-> Int Int)))
  (test-judgment-holds (types · (let ((x : Int 5))
             (let ((fn : (-> Int Int) (λ (n : Int) (- x n) : Int)))
               (fn 3))) Int))
  (test-judgment-holds (types · (let ((x : Int 5))
             (let ((fn : (-> Int Int) (λ (n : Int) (- x n) : Int)))
               (let ((x : Int 3))
                 (fn 3)))) Int))
  (test-judgment-holds (types · (λ (x : Int) (λ (n : Int) (+ n x) : Int) : (-> Int Int))
                              (-> Int (-> Int Int)))))
