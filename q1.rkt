#lang racket

(require racket/trace)
(provide sumUpRecursive sumUpTailRecursion)

(define (sumUpRecursive arr)
    (if (empty? arr)
        0
        (+ (first arr) (sumUpRecursive (rest arr)))))

(define (sumUpTailRecursion arr [ac 0])
    (if (empty? arr)
        ac
        (sumUpTailRecursion (rest arr) (+ ac (first arr)))))

(trace sumUpRecursive sumUpTailRecursion)
