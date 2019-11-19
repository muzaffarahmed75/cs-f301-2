#lang racket

(define (printPathsRecursive tree)
    (define (dfs g)
        (if (empty? (rest g))
            (list g)
            (map (lambda (x) (cons (first g) x)) (foldr append empty (map dfs (rest g))))))
    (for ([path (in-list (dfs tree))])
        (writeln path)))

(define (printPathsTailRecursive tree [path empty])
    (if (empty? (rest tree))
        (writeln (reverse (cons (first tree) path)))
        (for ([node (in-list (rest tree))])
            (printPathsTailRecursive node (cons (first tree) path)))))
