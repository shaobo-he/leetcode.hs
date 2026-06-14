#lang racket/base

(require racket/contract)
; Definition for singly-linked list:


; val : integer?
; next : (or/c list-node? #f)
(struct list-node
  (val next) #:mutable #:transparent)

; constructor
(define (make-list-node [val 0])
  (list-node val #f))

(define (ll->list node)
  (cond
    [(not node) '()]
    [else (cons node (ll->list (list-node-next node)))]))

(define (list->ll lst)
  (cond
    [(null? lst) #f]
    [else (list-node (car lst) (list->ll (cdr lst)))]))

(define/contract (odd-even-list head)
  (-> (or/c list-node? #f) (or/c list-node? #f))
  (cond
    [(not head) #f]
    [(not (list-node-next head)) head]
    [else
     (define odd head)
     (define even (list-node-next head))
     (define even-head even)
     (define (loop)
       (when (and even (list-node-next even))
         (set-list-node-next! odd (list-node-next even))
         (set! odd (list-node-next odd))
         (set-list-node-next! even (list-node-next odd))
         (set! even (list-node-next even))
         (loop)))
     (loop)
     (set-list-node-next! odd even-head)
     head]))

(module+ test
  (require rackunit)
  (define (node->vals n)
    (if n (cons (list-node-val n) (node->vals (list-node-next n))) '()))
  (check-equal? (node->vals (odd-even-list (list->ll '(1 2 3 4 5)))) '(1 3 5 2 4))
  (check-equal? (node->vals (odd-even-list (list->ll '(2 1 3 5 6 4 7)))) '(2 3 6 7 1 5 4))
  (check-equal? (odd-even-list #f) #f))