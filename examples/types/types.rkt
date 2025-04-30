#lang racket

(provide tc)

(define (store E x v)
  (cons (cons x v) E))

(define (lookup E x)
  (match E
    ['() (error "variable not found!")]
    [(cons (cons y v) E) (if (eq? x y) v
                             (lookup E x))]))

(define (tc-arith TE e1 e2)
  (match* ((tc TE e1) (tc TE e2))
    [('Int 'Int) 'Int]
    [(_    _)    (error "+ expects ints")]))

; TEnv -> Expr -> Type
(define (tc TE e)
  (match e
    [(? integer?)               'Int]
    [(? boolean?)               'Bool]
    [(? symbol?)                (lookup TE e)]
    [`(λ (,x : ,T1) : ,T2 ,e)   (if (equal? (tc (store TE x T1) e) T2)
                                    `(-> ,T1 ,T2)
                                    (error "lambda has wrong type"))]    
    [`(add1 ,e)                 (match (tc TE e)
                                  ['Int 'Int]
                                  [_    (error "add1 expects ints")])]
    [`(sub1 ,e)                 (match (tc TE e)
                                  ['Int 'Int]
                                  [_    (error "sub1 expects ints")])]
    [`(zero? ,e)                (match (tc TE e)
                                  ['Int 'Bool]
                                  [_    (error "zero? expects ints")])]
    [`(+ ,e1 ,e2)               (tc-arith TE e1 e2)]
    [`(- ,e1 ,e2)               (tc-arith TE e1 e2)]
    [`(* ,e1 ,e2)               (tc-arith TE e1 e2)]
    [`(/ ,e1 ,e2)               (tc-arith TE e1 e2)]
    [`(and ,e1 ,e2)             (let ((t1 (tc TE e1))
                                      (t2 (tc TE e2)))
                                  (if (equal? t1 t2) t1
                                      (error "and can only be applied on same types")))]
    [`(if ,e1 ,e2 ,e3)          (match (tc TE e1)
                                  ['Bool (let ((t1 (tc TE e1))
                                               (t2 (tc TE e2)))
                                           (if (equal? t1 t2) t1
                                               (error "both branches should have the same type")))]
                                  [_     (error "conditional should have bool type")])]
    [`(let ((,x : ,T ,e1)) ,e2) (if (equal? (tc TE e1) T)
                                    (tc (store TE x T) e2)
                                    (error "binder got the wrong type"))]
    [`(,e1 ,e2)                 (match (tc TE e1)
                                  [`(-> ,T1 ,T2) (if (equal? T1 (tc TE e2))
                                                     T2
                                                     (error "type error in fn app"))]
                                  [_ (error "expect fn type")])]
    [_                          (error "Parser error!")]))
