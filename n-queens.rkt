#lang racket/base

(require racket/contract)
(require racket/list)

(define (sol2str sol n)
  (define (line2str pos)
    (define row (make-string n #\.)) 
    (string-set! row (- pos 1) #\Q)
    row)
  (map line2str sol))

; for any two pos r,c, r',c'
; r + c != r' + c'
; r - c != r' - c'
; => abs (r - r') != abs(c - c')
(define (safe? candidate)
  (define (safe42? s s^)
    (not (= (abs (- (car s) (car s^)))
            (abs (- (cdr s) (cdr s^))))))
  (cond
    [(null? candidate) #t]
    [else
     (define s (car candidate))
     (define ss (cdr candidate))
     (and (andmap (λ (s^) (safe42? s s^)) ss)
          (safe? ss))]))

(define (insert x xs)
  (cond
    [(null? xs) (list (list x))]
    [else (cons
           (cons x xs)
           (map (λ (r) (cons (car xs) r)) (insert x (cdr xs))))]))

(define/contract (permutation nums)
  (-> (listof exact-integer?) (listof (listof exact-integer?)))
  (cond
    [(null? (cdr nums)) (list nums)]
    [else (append-map (λ (pm) (insert (car nums) pm))
                      (permutation (cdr nums)))]))

(define/contract (solve-n-queens n)
  (-> exact-integer? (listof (listof string?)))
  (define row (build-list n (λ (x) (+ 1 x))))
  (map (λ (s) (sol2str s n))
       (filter (λ (s) (safe? (map cons row s))) (permutation row))))
