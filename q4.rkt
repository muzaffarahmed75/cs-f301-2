#lang racket

(define (mergesort array)
    (define (sort l)
        (if (empty? (rest l))
            l
            (let ([n/2 (quotient (length l) 2)])
                (merge (sort (take l n/2)) (sort (drop l n/2))))))
    (define (merge a b)
        (cond
            [(empty? a) b]
            [(empty? b) a]
            [(< (first a) (first b)) (cons (first a) (merge (rest a) b))]
            [else (cons (first b) (merge a (rest b)))]))
    (sort array))
