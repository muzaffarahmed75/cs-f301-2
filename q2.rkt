#lang racket

(define (infixToPostfix infix)
    (define-values (left right c1 c2) (values #\( #\) (list #\+ #\-) (list #\*)))
    (define (conv in stack out)
        (if (empty? stack)
            out
            (let ([cur (first in)] [top (first stack)])
                (cond
                    [(number? cur) (conv (rest in) stack (cons cur out))]
                    [(equal? cur left) (conv (rest in) (cons cur stack) out)]
                    [(equal? cur right)
                        (if (equal? top left)
                            (conv (rest in) (rest stack) out)
                            (conv in (rest stack) (cons top out)))]
                    [(member cur c2)
                        (if (member top c2)
                            (conv in (rest stack) (cons top out))
                            (conv (rest in) (cons cur stack) out))]
                    [(member cur c1)
                        (if (member top (append c1 c2))
                            (conv in (rest stack) (cons top out))
                            (conv (rest in) (cons cur stack) out))]))))
    (reverse (conv (append infix (list right)) (list left) empty)))
