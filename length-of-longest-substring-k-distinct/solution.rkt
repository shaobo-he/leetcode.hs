#lang racket/base

(require racket/contract)
(require racket/list)

(define/contract (length-of-longest-substring-k-distinct s k)
  (-> string? exact-integer? exact-integer?)
  (define max-len 0)
  (define left 0)
  (define table (make-hash))
  (define (do t _)
    (define idx (car t))
    (define c (cdr t))
    (hash-set! table c idx)
    (if (> (hash-count table) k)
        (let*
            ([min-t (argmin cdr (hash->list table))]
             [min-c (car min-t)]
             [min-idx (cdr min-t)])
          (hash-remove! table min-c)
          (set! left (+ min-idx 1)))
        #f)
    (set! max-len (max max-len (+ 1 (- idx left)))))
  (define lst (string->list s))
  (foldl do #f (map cons (range (length lst)) lst))
  max-len)
(module+ test
  (require rackunit)
  (check-equal? (length-of-longest-substring-k-distinct "eceba" 2) 3)
  (check-equal? (length-of-longest-substring-k-distinct "aa" 1) 2)
  (check-equal? (length-of-longest-substring-k-distinct "abee" 1) 2))