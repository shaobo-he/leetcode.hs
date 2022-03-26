#lang racket/base

(require racket/contract)
(require racket/match)

(define/contract (unique-letter-string s)
  (-> string? exact-integer?)
  (define occurs (make-hash))
  (define count 0)
  (define len (string-length s))
  (foldl (λ (c i)
           (match
               (cond
                 [(not (hash-has-key? occurs c)) (cons -1 -1)]
                 [else (hash-ref occurs c)])
             [(cons ll l)
              (set! count (+ count (* (- l ll) (- i l))))
              (hash-set! occurs c (cons l i))])
           (+ i 1)) 0 (string->list s))
  (foldl (λ (t _)
           (match t
             [(cons ll l)
              (set! count (+ count (* (- l ll) (- len l))))]))
         #f (hash-values occurs))
  count)
