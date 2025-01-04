#lang racket
(provide main)
(require "interp.rkt")
 
;; String -> Void
;; Parse and interpret contents of given filename,
;; print result on stdout
(define (main fn)
  (let ([p (open-input-file fn)])
    (begin
      (read-line p) ; ignore #lang racket line
      (println (interp (read p)))
      (close-input-port p))))
