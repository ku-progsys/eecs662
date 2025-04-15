#lang racket

(require redex rackunit)

(provide L render-state-lang render-get render-update render-del render-eval-rules-judgment render-eval-rules-judgment-old)

(define (not-equal? v1 v2) (not (equal? v1 v2)))

(define-language L
  (E ::= ((x v) ...))
  (S ::= ((l v) ...))
  (x ::= variable-not-otherwise-mentioned)
  (l ::= (variable-prefix loc))
  (v ::= integer boolean l (Closure E e))
  (e ::= v x (λ (x) e) (add1 e) (sub1 e)
     (+ e e) (- e e) (* e e) (/ e e)
     (zero? e) (<= e e) (and e e)
     (if e e e)
     (let ((x e)) e)
     (e e) (seq e e)
     (new e) (deref e) (set e e) (free e)))

(define-metafunction L
  update : S l v -> S
  [(update ()                        l_1 v) ((l_1 v))]
  [(update ((l_1 v_1) (l_2 v_2) ...) l_1 v) ((l_1 v) (l_2 v_2) ...)]
  [(update ((l_1 v_1) (l_2 v_2) ...) l_3 v) ((l_1 v_1) (update ((l_2 v_2) ...) l_3 v))])

(define-metafunction L
  get : S l -> v
  [(get ((l_1 v_1) (l_2 v_2) ...) l_1) v_1]
  [(get ((l_1 v_1) (l_2 v_2) ...) l_3) (get ((l_2 v_2) ...) l_3)])

(define-metafunction L
  del : S l -> S
  [(del ((l_1 v_1) (l_2 v_2) ...) l_1) ((l_2 v_2) ...)]
  [(del ((l_1 v_1) (l_2 v_2) ...) l_3) ((l_1 v_1) (del ((l_2 v_2) ...) l_3))])

(define-judgment-form L
  #:mode (eval I I I O O)
  #:contract (eval E S e S v)

  [(eval E S e_1 S_1 v_1)
   (eval E S_1 e_2 S_2 v_2)
   ------------------------------- "add"
   (eval E S (+ e_1 e_2) S_2 ,(+ (term v_1) (term v_2)))]

  [---------------------- "lambda"
   (eval E S (λ (x) e) S (Closure E (λ (x) e)))]

  [(eval E S e_1 S_1 (Closure E_1 (λ (x) e_3)))
   (eval E S_1 e_2 S_2 v_2)
   (eval (store E_1 x v_2) S_2 e_3 S_3 v)
   -------------------------------------- "app"
   (eval E S (e_1 e_2) S_3 v)]

  [(eval E S e_1 S_1 v_1)
   (eval E S_1 e_2 S_2 v_2)
   ----------------------------- "seq"
   (eval E S (seq e_1 e_2) S_2 v_2)]

  [(eval E S e S_1 v)
   (where l ,(gensym 'loc))
   (where S_2 (update S_1 l v))
   ----------------------------- "new"
   (eval E S (new e) S_2 l)]

  [(eval E S e S_1 l)
   (where v (get S_1 l))
   ----------------------------- "deref"
   (eval E S (deref e) S_1 v)]

  [(eval E S e_1 S_1 l)
   (eval E S_1 e_2 S_2 v)
   (where S_3 (update S_2 l v))
   ----------------------------- "set"
   (eval E S (set e_1 e_2) S_3 v)]

  [(eval E S e S_1 l)
   (where S_2 (del S_1 l))
   ----------------------------- "free"
   (eval E S (free e) S_2 l)])

(define (renderer e)
  (with-compound-rewriters
      (['+          (λ (lws) (list "(+ "  (list-ref lws 2) " "   (list-ref lws 3) ")"))]
       ['-          (λ (lws) (list "(- "  (list-ref lws 2) " "   (list-ref lws 3) ")"))]
       ['*          (λ (lws) (list "(* "  (list-ref lws 2) " "   (list-ref lws 3) ")"))]
       ['/          (λ (lws) (list "(/ "  (list-ref lws 2) " "   (list-ref lws 3) ")"))]
       ['quotient   (λ (lws) (list "(/ "  (list-ref lws 2) " "   (list-ref lws 3) ")"))]
       ['<=         (λ (lws) (list "(<= " (list-ref lws 2) " "   (list-ref lws 3) ")"))]
       ['eval       (λ (lws) (list ""     (list-ref lws 2) " ⊢ ⟨" (list-ref lws 3) ", " (list-ref lws 4) "⟩ ⇓ ⟨" (list-ref lws 5) ", " (list-ref lws 6) "⟩" ))]
       ['not-equal? (λ (lws) (list ""     (list-ref lws 2) " ≠ " (list-ref lws 3) "" ))]
       ['equal?     (λ (lws) (list ""     (list-ref lws 2) " = " (list-ref lws 3) "" ))]
       ['or         (λ (lws) (list ""     (list-ref lws 2) " ∨ " (list-ref lws 3) "" ))]
       ['and        (λ (lws) (list ""     (list-ref lws 2) " ∧ " (list-ref lws 3) "" ))]
       ['not        (λ (lws) (list ""     " ¬ "            (list-ref lws 2) "" ))]
       ['not-in     (λ (lws) (list ""     (list-ref lws 2) " ∉ " (list-ref lws 3) "" ))]
       ['variable-prefix     (λ (lws) (list " locations " ))]
       ['gensym     (λ (lws) (list "fresh" ))]
       ;['lookup     (λ (lws) (list ""     (list-ref lws 2) "[" (list-ref lws 3) "]" "" ))]
       ;['store      (λ (lws) (list ""     (list-ref lws 2) "[" (list-ref lws 3) " ↦ " (list-ref lws 4) "]" "" ))]
       )
    (e)))

(define (render-state-lang)
  (renderer (λ () (parameterize ([metafunction-pict-style 'left-right/beside-side-conditions])
                    (render-language L)))))

(define (render-get)
  (renderer (λ () (parameterize ([metafunction-pict-style 'left-right/beside-side-conditions])
                    (render-metafunction get)))))

(define (render-update)
  (renderer (λ () (parameterize ([metafunction-pict-style 'left-right/beside-side-conditions])
                    (render-metafunction update)))))

(define (render-del)
  (renderer (λ () (parameterize ([metafunction-pict-style 'left-right/beside-side-conditions])
                    (render-metafunction del)))))

(define (render-eval-rules-judgment)
  (renderer (λ () (parameterize ([judgment-form-cases '("seq"
                                                        "new"
                                                        "deref"
                                                        "set"
                                                        "free")])
                    (render-judgment-form eval)))))

(define (render-eval-rules-judgment-old)
  (renderer (λ () (parameterize ([judgment-form-cases '("add"
                                                        "lambda"
                                                        "app")])
                    (render-judgment-form eval)))))
