#lang racket

(provide cg)

(struct EqC (lhs rhs) #:prefab)


(define (cg e)
  (match e
    [(? integer?)             (list (EqC e 'Int))]
    [(? boolean?)             (list (EqC e 'Bool))]
    [(? symbol?)              (list (EqC e e))]
    [`(λ (,x) ,e1)            (append (cg e1)
                                      (list (EqC e `(-> ,x ,e1))))]
    #|[`(add1 ,e)               (match (tc TE e)
                                ['Int 'Int]
                                [_    (error "add1 expects ints")])]
    [`(sub1 ,e)               (match (tc TE e)
                                ['Int 'Int]
                                [_    (error "sub1 expects ints")])]
    [`(zero? ,e)              (match (tc TE e)
                                ['Int 'Bool]
                                [_    (error "zero? expects ints")])]|#

    
    [`(+ ,e1 ,e2)             (append (cg e1)
                                      (cg e2)
                                      (list (EqC e1 'Int)
                                            (EqC e2 'Int)
                                            (EqC e  'Int)))]
    #|[`(- ,e1 ,e2)             (tc-arith TE e1 e2)]
    [`(* ,e1 ,e2)             (tc-arith TE e1 e2)]
    [`(/ ,e1 ,e2)             (tc-arith TE e1 e2)]
    [`(and ,e1 ,e2)           (let ((t1 (tc TE e1))
                                    (t2 (tc TE e2)))
                                (if (equal? t1 t2) t1
                                    (error "and can only be applied on same types")))]
    [`(if ,e1 ,e2 ,e3)        (match (tc TE e1)
                                ['Bool (let ((t1 (tc TE e1))
                                             (t2 (tc TE e2)))
                                         (if (equal? t1 t2) t1
                                             (error "both branches should have the same type")))]
                                [_     (error "conditional should have bool type")])]
    [`(let ((,x : ,T ,e1)) ,e2) (if (equal? (tc TE e1) T)
                                  (tc (store TE x T) e2)
                                  (error "binder got the wrong type"))]|#
    [`(,e1 ,e2)               (append (cg e1)
                                      (cg e2)
                                      (list (EqC e1 `(-> ,e2 ,e))))]
    [_                       (error "Parser error!")]))

#|
(define (unify cs substs)
  (match cs
    ['() substs]
    [(cons c cs) (let ((lhs (car c))
                       (rhs (cdr c)))
                   (if (and (symbol? lhs) (not (memq lhs '(Int Bool)))) ; variable
                       |#
