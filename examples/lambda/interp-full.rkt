#lang racket

(provide free? alpha-reduce beta-reduce interp)

; Listof Var -> Var -> Expr -> Bool
(define (free? xs x e)
  (match e
    [(? integer?) #f]
    [(? symbol?)  (and (eq? e x) (not (memq x xs)))]
    [`(λ (,y) ,e) (free? (cons y xs) x e)]
    [`(zero? ,e) (free? xs x e)]
    [`(- ,e1 ,e2) (or (free? xs x e1) (free? xs x e2))]
    [`(* ,e1 ,e2) (or (free? xs x e1) (free? xs x e2))]
    [`(if ,e1 ,e2 ,e3) (or (free? xs x e1) (free? xs x e2) (free? xs x e3))]
    [`(let ((,y ,e1)) ,e2) (or (free? xs x e1) (free? (cons y xs) x e2))]
    [`(,e1 ,e2)   (or (free? xs x e1) (free? xs x e2))]))

; Expr -> Var -> Var -> Expr
; z has to be fresh
(define (alpha-reduce e x z)
  (match e
    [(? integer?) e]
    [(? symbol?)  (if (eq? x e) z e)]
    [`(λ (,y) ,e) (if (eq? x y)
                      `(λ (,x) ,e)
                      `(λ (,y) ,(alpha-reduce e x z)))]
    [`(zero? ,e) `(zero? ,(alpha-reduce e x z))]
    [`(- ,e1 ,e2) `(- ,(alpha-reduce e1 x z) ,(alpha-reduce e2 x z))]
    [`(* ,e1 ,e2) `(* ,(alpha-reduce e1 x z) ,(alpha-reduce e2 x z))]
    [`(if ,e1 ,e2 ,e3) `(if ,(alpha-reduce e1 x z) ,(alpha-reduce e2 x z) ,(alpha-reduce e3 x z))]
    [`(let ((,y ,e1)) ,e2) (if (eq? x y)
                               `(let ((,x ,(alpha-reduce e1 x z))) ,e2)
                               `(let ((,y ,(alpha-reduce e1 x z))) ,(alpha-reduce e2 x z)))]
    [`(,e1 ,e2)   `(,(alpha-reduce e1 x z) ,(alpha-reduce e2 x z))]))

; Expr -> Var -> Expr -> Expr
(define (beta-reduce e1 x e2)
  (match e1
    [(? integer?) e1]
    [(? symbol?)  (if (eq? e1 x) e2 e1)]
    [`(λ (,y) ,e) (if (eq? x y)
                      `(λ (,y) ,e)
                      (if (not (free? '() y e2))
                          `(λ (,y) ,(beta-reduce e x e2))
                          (let* ((ry (gensym))
                                 (re (alpha-reduce e y ry)))
                            `(λ (,ry) ,(beta-reduce re x e2)))))]
    [`(zero? ,e) `(zero? ,(beta-reduce e x e2))]
    [`(- ,ea1 ,ea2) `(- ,(beta-reduce ea1 x e2) ,(beta-reduce ea2 x e2))]
    [`(* ,ea1 ,ea2) `(* ,(beta-reduce ea1 x e2) ,(beta-reduce ea2 x e2))]
    [`(if ,ea1 ,ea2 ,ea3) `(if ,(beta-reduce ea1 x e2) ,(beta-reduce ea2 x e2) ,(beta-reduce ea3 x e2))]
    [`(let ((,y ,ea1)) ,ea2) (if (eq? x y)
                               `(let ((,x ,(beta-reduce ea1 x e2))) ,ea2)
                               `(let ((,y ,(beta-reduce ea1 x e2))) ,(beta-reduce ea2 x e2)))]
    [`(,ea1 ,ea2) `(,(beta-reduce ea1 x e2) ,(beta-reduce ea2 x e2))]))

; Expr -> Val
(define (interp e)
  (match e
    [`(zero? ,e)             (eq? (interp e) 0)]
    [`(- ,ea1 ,ea2)          (- (interp ea1) (interp ea2))]
    [`(* ,ea1 ,ea2)          (* (interp ea1) (interp ea2))]
    [`(if ,ea1 ,ea2 ,ea3)    (match (interp ea1)
                               [#f (interp ea3)]
                               [_  (interp ea2)])]
    [`(let ((,x ,ea1)) ,ea2) (interp (beta-reduce ea2 x ea1))]
    [`(,e1 ,e2)              (match (interp e1)
                               [`(λ (,x) ,e) (interp (beta-reduce e x e2))]
                               [(? symbol? x) x])]
    [_                       e]))
