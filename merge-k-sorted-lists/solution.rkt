#lang typed/racket/base

(require racket/list
         "../merge-two-sorted-lists/solution.rkt")

(: merge (-> (Listof (Listof Integer)) (Listof Integer)))
(define (merge lsts)
  (define len (length lsts))
  (cond
    [(= len 0) '()]
    [(= len 1) (car lsts)]
    [else (let-values
              ([(fst-half snd-half) (split-at lsts (quotient len 2))])
            (merge/two-lists (merge fst-half)
                             (merge snd-half)))]))


(module+ test
  (require typed/rackunit)
  (check-equal? (merge '((1 4 5) (1 3 4) (2 6))) '(1 1 2 3 4 4 5 6))
  (check-equal? (merge '()) '())
  (check-equal? (merge '((1) (0))) '(0 1)))
