#lang racket/base
(require "streams.rkt")

(define input-prompt ";;; Query inputs: ")
(define output-prompt ";;; Results output")

(define (query-driver-loop)
  (prompt-for-input input-prompt)
  (let (q (process-query-syntax (read)))
    (cond ((add-assertion? q)
           (add-rule-or-assertion! (get-assertion-body q))
           (newline)
           (display "Assertion added"))
          (else
           (newline)
           (display output-prompt)
           (map-stream
            (lambda (frame)
              (instantiate q
                           frame
                           (lambda (v f)
                             (contract-question-mark v))))
            (q-eval q (singleton-stream '())))
           (query-driver-loop)))))
  
