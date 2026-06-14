#lang typed/racket/base

;; Scan left to right, checking for (sum - elem) among the indices already seen.
;; Looking only at earlier indices guarantees the two indices are distinct
;; (so an element is never paired with itself).
(: two-sum (-> (Vectorof Integer) Integer (Pairof Integer Integer)))
(define (two-sum lst sum)
  (define len (vector-length lst))
  (let loop ([idx : Integer 0]
             [seen : (Immutable-HashTable Integer Integer) (hash)])
    (cond
      [(>= idx len) (error "no such pair!")]
      [else
       (define elem (vector-ref lst idx))
       (define complement (- sum elem))
       (cond
         [(hash-has-key? seen complement) (cons (hash-ref seen complement) idx)]
         [else (loop (add1 idx) (hash-set seen elem idx))])])))

(module+ test
  (require typed/rackunit)
  (check-equal? (two-sum (vector 2 7 11 15) 9) (cons 0 1))
  (check-equal? (two-sum (vector 1 5 8) 13) (cons 1 2))
  ;; previously returned (0 . 0) by pairing 3 with itself
  (check-equal? (two-sum (vector 3 2 4) 6) (cons 1 2))
  ;; duplicate values must use both occurrences
  (check-equal? (two-sum (vector 3 3) 6) (cons 0 1)))
