#lang racket

(define (mergesort array)
    ; The complexity of merge is O(n+m), where n in the size of a and m is the
    ; size of b. This is since merge recursively calls itself with at least one
    ; of the lists shorter by at least one element.
    (define (merge a b)
        (cond
            ; Checking if a list is empty is O(1).
            [(empty? a) b]
            [(empty? b) a]
            ; Fetching the first element or the rest of a list takes constant
            ; time in Lisp since a list is nothing but a pair, the first
            ; element of pair being the first item in the list.
            ; Likewise, construction of a list (or more appropriately, pair)
            ; is also constant time.
            [(< (first a) (first b)) (cons (first a) (merge (rest a) b))]
            [else (cons (first b) (merge a (rest b)))]))
    (define (sort l)
        (if (empty? (rest l))
            l
            (let ([n/2 (quotient (length l) 2)])
                ; take and drop have linear time complexity.
                ; Merge is O(n) where n is the size of the lists combined.
                ; The complexity of sort function is then T(n), where
                ; T(n) = 2(n/2) + O(n)
                ; Where O(n) is the combined complexity of take, drop, length
                ; and passing the lists recursively.
                ; The equation can be solved for T(n), giving T(n) = O(n lg n)
                (merge (sort (take l n/2)) (sort (drop l n/2))))))
    (sort array))