#lang racket/base

;; Single pass: track the lowest price seen so far and the best profit.
(define (max-profit prices)
  (cond
    [(null? prices) 0]
    [else
     (let loop ([prices (cdr prices)] [lowest (car prices)] [best 0])
       (cond
         [(null? prices) best]
         [else
          (define p (car prices))
          (loop (cdr prices) (min lowest p) (max best (- p lowest)))]))]))

(module+ test
  (require rackunit)
  (check-equal? (max-profit '(7 1 5 3 6 4)) 5)
  (check-equal? (max-profit '(7 6 4 3 1)) 0)
  (check-equal? (max-profit '()) 0))
