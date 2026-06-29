#lang racket

;; LeetCode 31: Next Permutation — purely functional recast over a list of ints.
;;
;; find-pivot splits xs as  pre ++ (p . suf)  where suf is the maximal weakly
;; decreasing suffix and p < (car suf) (the rightmost ascent); #f means the whole
;; list is weakly decreasing (maximal → wrap to minimal by reversing).
;; swap-last replaces the rightmost element of suf that is > p with p, returning
;; (cons g new-suf); reversing the modified suffix yields the ascending tail.

;; -> (list pre p suf)  or  #f
(define (find-pivot xs)
  (cond
    [(null? xs) #f]
    [(null? (cdr xs)) #f]
    [else
     (define x (car xs))
     (define y (cadr xs))
     (define sub (find-pivot (cdr xs)))
     (if sub
         (list (cons x (first sub)) (second sub) (third sub))
         (if (< x y) (list '() x (cdr xs)) #f))]))

;; -> (cons g new-suffix)  or  #f
(define (swap-last p xs)
  (cond
    [(null? xs) #f]
    [else
     (define c (car xs))
     (define rest (cdr xs))
     (define sub (swap-last p rest))
     (if sub
         (cons (car sub) (cons c (cdr sub)))
         (if (< p c) (cons c (cons p rest)) #f))]))

(define (next-permutation xs)
  (define piv (find-pivot xs))
  (if (not piv)
      (reverse xs)
      (let ([pre (first piv)] [p (second piv)] [suf (third piv)])
        (define sw (swap-last p suf))
        (if (not sw)
            (reverse xs)
            (append pre (cons (car sw) (reverse (cdr sw))))))))

(module+ test
  (require rackunit)
  (check-equal? (next-permutation '(1 2 3)) '(1 3 2))
  (check-equal? (next-permutation '(3 2 1)) '(1 2 3))   ; wrap
  (check-equal? (next-permutation '(1 1 5)) '(1 5 1))
  (check-equal? (next-permutation '(1 3 2)) '(2 1 3))
  (check-equal? (next-permutation '(1 5 1)) '(5 1 1)))
