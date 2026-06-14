#lang racket/base

;; Merge two sorted lists (cf. the dependently-typed Vect version in solution.idr).
(define (sorted-merge xs ys)
  (cond
    [(null? xs) ys]
    [(null? ys) xs]
    [(<= (car xs) (car ys)) (cons (car xs) (sorted-merge (cdr xs) ys))]
    [else (cons (car ys) (sorted-merge xs (cdr ys)))]))

(module+ test
  (require rackunit)
  (check-equal? (sorted-merge '(1 2 3) '(2 5 6)) '(1 2 2 3 5 6))
  (check-equal? (sorted-merge '(1 3 4) '(2 5 6)) '(1 2 3 4 5 6))
  (check-equal? (sorted-merge '() '(1)) '(1))
  (check-equal? (sorted-merge '(1) '()) '(1)))
