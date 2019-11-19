#lang racket

(define (evalPostfixExpr postfix)
    (define (eval expr stack)
        (if (empty? expr)
            (first stack)
            (let ([cur (first expr)] [rem (rest expr)])
                (cond
                    [(number? cur) (eval rem (cons cur stack))]
                    [(equal? cur #\*) (eval rem (cons (* (cadr stack) (car stack)) (cddr stack)))]
                    [(equal? cur #\+) (eval rem (cons (+ (cadr stack) (car stack)) (cddr stack)))]
                    [(equal? cur #\-) (eval rem (cons (- (cadr stack) (car stack)) (cddr stack)))]))))
    (eval postfix empty))
