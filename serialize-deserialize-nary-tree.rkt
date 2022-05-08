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
