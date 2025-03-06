#lang racket

(require rackunit)

(provide interp)

;; Expr -> Value
;; Interpret given expression
(define (interp e)
  (match e
    [(? integer?)          e]
    [(? boolean?)          e]
    [(? symbol?)           (error "Unbound variable:" (symbol->string e))]
    ; ...
    ; other cases omitted for brevity
    [`(let ((,x ,e1)) ,e2) (interp-let x e1 e2)]
    [_                     (error "Parser error!")]))

;; other definitions omitted for brevity

(define (interp-let x e1 e2)
  (let ((e3 (subst e2 x (interp e1))))
    (interp e3)))

(define (subst in x with)
  (match in
    [(? integer?)          in]
    [(? boolean?)          in]
    [(? symbol?)           (if (eq? in x) with in)]
    [`(add1 ,e)            `(add1 ,(subst e x with))]
    [`(sub1 ,e)            `(sub1 ,(subst e x with))]
    [`(+ ,e1 ,e2)          `(+ ,(subst e1 x with) ,(subst e2 x with))]
    [`(- ,e1 ,e2)          `(- ,(subst e1 x with) ,(subst e2 x with))]
    [`(* ,e1 ,e2)          `(* ,(subst e1 x with) ,(subst e2 x with))]
    [`(/ ,e1 ,e2)          `(/ ,(subst e1 x with) ,(subst e2 x with))]
    [`(zero? ,e)           `(zero? ,(subst e x with))]
    [`(and ,e1 ,e2)        `(and ,(subst e1 x with) ,(subst e2 x with))]
    [`(<= ,e1 ,e2)         `(<=  ,(subst e1 x with) ,(subst e2 x with))]
    [`(if ,e1 ,e2 ,e3)     `(if ,(subst e1 x with) ,(subst e2 x with) ,(subst e3 x with))]
    [`(let ((,y ,e1)) ,e2) (if (eq? x y)
                               `(let ((,y ,(subst e1 x with))) ,e2)
                               `(let ((,y ,(subst e1 x with))) ,(subst e2 x with)))]
    [_                      (error "Parser error!")]))
