#lang racket
(provide (all-defined-out))
(require (for-syntax racket/runtime-path racket/base racket/file pkg/lib)
         (for-meta 2 racket/base pkg/lib))
(require scribble/manual racket/runtime-path pkg/lib)
(require images/icons/file)

(define ((make-codeblock-include ctxt) fn) ;; Should h be ctxt!?  Seems like a bug
   (filebox (link (string-append "examples/" fn) (tt fn))
            (typeset-code #:context #'h (file->string (build-path "examples" fn)))))

;; calls proc and produces stdout appendde w/ stderr as a string
(define (with-output-to-string/err proc)
  (define os "")
  (define es "")
  (define r #f)
  (set! os (call-with-output-string
            (lambda (o)
              (set! es
                    (call-with-output-string
                     (λ (e)
                       (parameterize ([current-output-port o]
                                      [current-error-port e])
                         (set! r (proc)))))))))
  ; (unless r (error (string-append os es)))
  (string-append os es))

(define (shell-result c)
  (with-output-to-string/err (λ () (system #:set-pwd? #t c))))

(define (shell . cs)
  (match cs
    ['() ""]
    [(cons c cs)
     (string-append "> " c "\n"
                    (with-output-to-string/err (λ () (system #:set-pwd? #t c)))
                    (apply shell cs))]))
