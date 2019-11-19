#lang racket

(define (printPathsRecursive tree)
    (define (dfs g)
        (if (empty? (rest g))
            (list g)
            (map (lambda (x) (cons (first g) x)) (foldr append empty (map dfs (rest g))))))
    (for ([path (in-list (dfs tree))])
        (writeln path)))

(define (printPathsTailRecursive tree)
    (define (dfs g path)
        (if (empty? (rest g))
            (writeln (reverse (cons (first g) path)))
            (for ([node (in-list (rest g))])
                (dfs node (cons (first g) path)))))
    (dfs tree empty))
