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