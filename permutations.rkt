#lang racket/base

(require racket/contract)
(require racket/list)

;; (insert 1 '(2 3)) -> '(1 2 3) '(2 1 3) '(2 3 1)
;; (insert 1 '(3)) -> '(1 3) '(3 1)
(define (insert x xs)
  (cond
    [(null? xs) (list (list x))]
    [else (cons
           (cons x xs)
           (map (λ (r) (cons (car xs) r)) (insert x (cdr xs))))]))

(define/contract (permute/insert nums)
  (-> (listof exact-integer?) (listof (listof exact-integer?)))
  (cond
    [(null? (cdr nums)) (list nums)]
    [else (append-map (λ (pm) (insert (car nums) pm))
                      (permute/insert (cdr nums)))]))

;; (pick '(1 2 3)) -> '(1 2 3) '(2 1 3) '(3 1 2)
;; (pick '(2 3)) -> '(2 3) '(3 2)
(define (pick lst)
  (cond
    [(null? (cdr lst)) (list lst)]
    [else (cons lst (map (λ (t) (cons (car t) (cons (car lst) (cdr t)))) (pick (cdr lst))))]
  ))

;; (permute '(1 2 3)) -> '(1 2 3) '(1 3 2) '(2 1 3) '(2 3 1) '(3 1 2) '(3 2 1)
;; (permute '(2 3)) -> '(2 3) '(3 2)
(define/contract (permute/pick nums)
  (-> (listof exact-integer?) (listof (listof exact-integer?)))
  (cond
    [(null? (cdr nums)) (list nums)]
    [else (append-map
           (λ (t)
             (map
              (λ (xs) (cons (car t) xs))
              (permute/pick (cdr t))))
           (pick nums))]))