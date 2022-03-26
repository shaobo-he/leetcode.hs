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
(length-of-longest-substring-k-distinct "abee" 1)