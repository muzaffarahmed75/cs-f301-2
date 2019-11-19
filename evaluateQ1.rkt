#lang racket

(require racket/trace)
(require "q1.rkt")

(define (random-list n mx)
    (if (zero? n)
        empty
        (cons (random mx) (random-list (sub1 n) mx))))

(define rgl (random-list (+ 5 (random 5)) (+ 10 (random 40))))

(trace-call 'sur sumUpRecursive rgl)
(trace-call 'sutr sumUpTailRecursion rgl)
