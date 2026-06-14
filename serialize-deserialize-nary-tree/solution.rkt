#lang racket/base

(require racket/match)
(require racket/string)

(define (serialize t)
  (match t
    ['() ""]
    [(cons n cs)
     (string-append
      "("
      (number->string n)
      (string-join
       (map
        serialize
        cs)
       "")
      ")")]))

(define (deserialize str)
  (read (open-input-string str)))

(module+ test
  (require rackunit)
  (define tree '(1 (3 (5) (6)) (2) (4)))
  (check-equal? (serialize tree) "(1(3(5)(6))(2)(4))")
  (check-equal? (deserialize (serialize tree)) tree)   ;; round-trip
  (check-equal? (serialize '()) "")
  (check-equal? (serialize '(1)) "(1)"))
