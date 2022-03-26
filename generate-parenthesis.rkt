#lang racket/base

(require racket/contract)
(define/contract (generate-parenthesis n)
  (-> exact-integer? (listof string?))
  (define res '())
  (define (do l r t)
    (if (and (= l n) (= r n))
        (set! res (cons t res))
        (begin (if (< l n)
                   (do (+ l 1) r (string-append t "("))
                   #f)
               (if (< r l)
                   (do l (+ r 1) (string-append t ")"))
                   #f))))
  (do 0 0 "")
  res)

(generate-parenthesis 4)