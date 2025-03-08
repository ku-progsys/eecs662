#lang racket

(provide free? alpha-reduce beta-reduce)

; Listof Var -> Var -> Expr -> Bool
(define (free? xs x e)
  (match e
    [(? symbol?)  (and (eq? e x) (not (memq x xs)))]
    [`(λ (,y) ,e) (free? (cons y xs) x e)]
    [`(,e1 ,e2)   (or (free? xs x e1) (free? xs x e2))]))


; Expr -> Var -> Var -> Expr
; z has to be fresh
(define (alpha-reduce e x z)
  (match e
    [(? symbol?)  (if (eq? x e) z e)]
    [`(λ (,y) ,e) (if (eq? x y)
                      `(λ (,x) ,e)
                      `(λ (,y) ,(alpha-reduce e x z)))]
    [`(,e1 ,e2)   `(,(alpha-reduce e1 x z) ,(alpha-reduce e2 x z))]))

; Expr -> Var -> Expr -> Expr
(define (beta-reduce e1 x e2)
  (match e1
    [(? symbol?)  (if (eq? e1 x) e2 e1)]
    [`(λ (,y) ,e) (if (eq? x y)
                      `(λ (,y) ,e)
                      (if (not (free? '() y e2))
                          `(λ (,y) ,(beta-reduce e x e2))
                          (let* ((ry (gensym))
                                 (re (alpha-reduce e y ry)))
                            `(λ (,ry) ,(beta-reduce re x e2)))))]
    [`(,ea1 ,ea2) `(,(beta-reduce ea1 x e2) ,(beta-reduce ea2 x e2))]))
