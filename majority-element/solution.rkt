#lang racket/base

;; LeetCode 169: Majority Element — Boyer–Moore voting.
;; A single left fold carrying (candidate, count).  Returns the candidate
;; (#f for the empty list).  On inputs that actually have a strict majority
;; (the LeetCode guarantee) the candidate IS that majority element.
(define (majority-element nums)
  (for/fold ([cand #f] [count 0] #:result cand)
            ([x (in-list nums)])
    (cond
      [(= count 0)        (values x 1)]
      [(equal? x cand)    (values cand (add1 count))]
      [else               (values cand (sub1 count))])))

(module+ test
  (require rackunit)
  (check-equal? (majority-element '(3 2 3)) 3)
  (check-equal? (majority-element '(2 2 1 1 1 2 2)) 2)
  (check-equal? (majority-element '(1)) 1)
  (check-equal? (majority-element '()) #f))
