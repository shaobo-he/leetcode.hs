#lang typed/racket/base

(require racket/vector)

(provide (rename-out [merge merge/two-lists]))

;; natural recusion
(: merge (-> (Listof Integer) (Listof Integer) (Listof Integer)))
(define (merge lst1 lst2)
  (cond
    [(null? lst1) lst2]
    [(null? lst2) lst1]
    [else (let ([head1 (car lst1)]
                [head2 (car lst2)])
            (cond
              [(< head1 head2) (cons head1 (merge (cdr lst1) lst2))]
              [else (cons head2 (merge lst1 (cdr lst2)))]))]))

(module+ test
  (require typed/rackunit)
  (check-equal? (merge '(1 2 4) '(1 3 4)) '(1 1 2 3 4 4))
  (check-equal? (merge '() '(0)) '(0))
  (check-equal? (merge '() '()) '()))