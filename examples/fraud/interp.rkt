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
    [`(add1 ,e)            (+ (only-int (interp e))  1)]
    [`(sub1 ,e)            (- (only-int (interp e))  1)]
    [`(+ ,e1 ,e2)          (+ (only-int (interp e1)) (only-int (interp e2)))]
    [`(- ,e1 ,e2)          (- (only-int (interp e1)) (only-int (interp e2)))]
    [`(* ,e1 ,e2)          (* (only-int (interp e1)) (only-int (interp e2)))]
    [`(/ ,e1 ,e2)          (interp-div e1 e2)]
    [`(zero? ,e)           (interp-zero? e)]
    [`(and ,e1 ,e2)        (interp-and e1 e2)]
    [`(<= ,e1 ,e2)         (<= (only-int (interp e1)) (only-int (interp e2)))]
    [`(if ,e1 ,e2 ,e3)     (interp-if e1 e2 e3)]
    [`(let ((,x ,e1)) ,e2) (interp-let x e1 e2)]
    [_                     (error "Parser error!")]))

(define (interp-div e1 e2)
  (match* ((only-int (interp e1)) (only-int (interp e2)))
    [(v1 0)  (error "Division by 0 not allowed!")]
    [(v1 v2) (quotient v1 v2)]))

(define (only-int v)
  (if (integer? v)
      v
      (error "Integer expected!")))

(define (interp-zero? e)
  (match (interp e)
    [0 #t]
    [_ #f]))

(define (interp-and e1 e2)
  (match (interp e1)
    [#f #f]
    [_  (interp e2)]))

(define (interp-if e1 e2 e3)
  (match (interp e1)
    [#f (interp e3)]
    [_  (interp e2)]))

(define (interp-let x e1 e2)
  (interp (subst e2 x (interp e1))))

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

(module+ test
  (check-eqv? (interp '(let ((x 1)) (+ x 3))) 4)
  (check-eqv? (interp '(let ((x 1))
                         (let ((y 2))
                           (+ x y)))) 3)
  (check-eqv? (interp '(let ((x (add1 6)))
                         (let ((x (+ 6 x)))
                           (/ x 2)))) 6)
  (check-exn exn:fail? (λ ()
                         (interp '(let ((x (add1 6)))
                                    (let ((x (+ 6 x)))
                                      (/ x y)))))))