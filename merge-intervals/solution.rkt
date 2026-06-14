#lang racket/base

(require racket/list)

;; Sort by start, then fold coalescing overlaps. Intervals are (lo hi) lists.
(define (merge-intervals intervals)
  (cond
    [(null? intervals) '()]
    [else
     (define sorted (sort intervals < #:key car))
     (reverse
      (foldl (lambda (iv acc)
               (define lo (car iv)) (define hi (cadr iv))
               (define top (car acc))
               (if (<= lo (cadr top))
                   (cons (list (car top) (max (cadr top) hi)) (cdr acc))
                   (cons iv acc)))
             (list (car sorted))
             (cdr sorted)))]))

(module+ test
  (require rackunit)
  (check-equal? (merge-intervals '((1 3) (2 6) (8 10) (15 18)))
                '((1 6) (8 10) (15 18)))
  (check-equal? (merge-intervals '((1 4) (4 5))) '((1 5)))
  (check-equal? (merge-intervals '()) '()))
