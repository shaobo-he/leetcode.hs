#lang racket/base

;; Sliding window: last-seen index per char; jump the window start past any repeat.
(define (length-of-longest-substring s)
  (define last-seen (make-hash))
  (let loop ([chars (string->list s)] [i 0] [start 0] [best 0])
    (cond
      [(null? chars) best]
      [else
       (define c (car chars))
       (define start*
         (if (hash-has-key? last-seen c)
             (max start (add1 (hash-ref last-seen c)))
             start))
       (hash-set! last-seen c i)
       (loop (cdr chars) (add1 i) start* (max best (add1 (- i start*))))])))

(module+ test
  (require rackunit)
  (check-equal? (length-of-longest-substring "abcabcbb") 3)
  (check-equal? (length-of-longest-substring "bbbbb") 1)
  (check-equal? (length-of-longest-substring "pwwkew") 3)
  (check-equal? (length-of-longest-substring "") 0))
