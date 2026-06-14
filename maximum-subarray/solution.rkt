#lang racket/base

;; Kadane's algorithm.
(define (max-sub-array nums)
  (cond
    [(null? nums) 0]
    [else
     (let loop ([nums (cdr nums)] [cur (car nums)] [best (car nums)])
       (cond
         [(null? nums) best]
         [else
          (define cur* (max (car nums) (+ cur (car nums))))
          (loop (cdr nums) cur* (max best cur*))]))]))

(module+ test
  (require rackunit)
  (check-equal? (max-sub-array '(-2 1 -3 4 -1 2 1 -5 4)) 6)
  (check-equal? (max-sub-array '(1)) 1)
  (check-equal? (max-sub-array '(5 4 -1 7 8)) 23))
