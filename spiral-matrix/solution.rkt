#lang racket/base

(require racket/contract)

;; based on: https://leetcode.com/problems/spiral-matrix/discuss/20571/1-liner-in-Python-%2B-Ruby
;; beautiful solution 🤩

(define (rotate matrix)
  (cond
    [(null? matrix) '()]
    [else (reverse (apply map list matrix))]))
  
(define/contract (spiral-order matrix)
  (-> (listof (listof exact-integer?)) (listof exact-integer?))
  (cond
    [(null? matrix) '()]
    [else (append (car matrix) (spiral-order (rotate (cdr matrix))))]))

(module+ test
  (require rackunit)
  (check-equal? (spiral-order '((1 2 3) (4 5 6) (7 8 9)))
                '(1 2 3 6 9 8 7 4 5))
  (check-equal? (spiral-order '((1 2 3 4) (5 6 7 8) (9 10 11 12)))
                '(1 2 3 4 8 12 11 10 9 5 6 7))
  (check-equal? (spiral-order '()) '()))