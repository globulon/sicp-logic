#lang racket/base
(provide cons-stream)

(define (delay b)
  (lambda () b))

(define (force f) (f))

(define (cons-stream a b)
  (cons a (delay b)))

(define (car-stream s)
  (car s))

(define (cdr-stream s)
  (force (cdr s)))

(define (cadr-stream s)
  (car-stream (cdr-stream s)))

(define null-stream? null?)

(define (map-stream f s)
  (cond ((null-stream? s) null?)
        ( else
         (cons-stream (f (car-stream s))
                      (map-stream f (cdr-stream s))))))

(define (for-each-stream f s)
  (cond ((null-stream? s) 'done)
        (else
         (begin (f (car-stream s))
                (for-each-stream f (cdr-stream s))))))

(define (display-line l)
  (display l)
  (newline))

(define (display-stream s)
  (for-each-stream display-line s))