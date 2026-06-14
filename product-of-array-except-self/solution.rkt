#lang racket/base

;; result[i] = (product of everything left of i) * (product of everything right).
(define (product-except-self nums)
  (define n (length nums))
  (cond
    [(= n 0) '()]
    [else
     ;; prefix[i] = product of nums[0..i-1]  (length n+1, last dropped by zip)
     (define prefix
       (reverse (foldl (lambda (x acc) (cons (* (car acc) x) acc)) '(1) nums)))
     ;; suffix[i] = product of nums[i..]      ((cdr suffix)[i] = product right of i)
     (define suffix
       (foldr (lambda (x acc) (cons (* (car acc) x) acc)) '(1) nums))
     (for/list ([p (in-list prefix)] [s (in-list (cdr suffix))])
       (* p s))]))

(module+ test
  (require rackunit)
  (check-equal? (product-except-self '(1 2 3 4)) '(24 12 8 6))
  (check-equal? (product-except-self '(0 0)) '(0 0))
  (check-equal? (product-except-self '(0 1)) '(1 0))
  (check-equal? (product-except-self '(5 2 3)) '(6 15 10)))
