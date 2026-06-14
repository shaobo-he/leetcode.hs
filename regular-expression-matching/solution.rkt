#lang racket/base

(require racket/match)

;; Brzozowski derivatives (port of the Haskell solution).
(struct re:empty () #:transparent)   ; matches nothing
(struct re:eps   () #:transparent)   ; matches the empty string
(struct re:dot   () #:transparent)   ; matches any one char
(struct re:chr   (c) #:transparent)
(struct re:seq   (a b) #:transparent)
(struct re:alt   (a b) #:transparent)
(struct re:star  (r) #:transparent)

(define (nullable? r)
  (match r
    [(re:empty) #f]
    [(re:eps) #t]
    [(re:dot) #f]
    [(re:chr _) #f]
    [(re:seq a b) (and (nullable? a) (nullable? b))]
    [(re:alt a b) (or (nullable? a) (nullable? b))]
    [(re:star _) #t]))

;; derivative of r with respect to character c
(define (deriv c r)
  (match r
    [(re:empty) (re:empty)]
    [(re:eps) (re:empty)]
    [(re:dot) (re:eps)]
    [(re:chr c^) (if (char=? c c^) (re:eps) (re:empty))]
    [(re:alt a b) (re:alt (deriv c a) (deriv c b))]
    [(re:seq a b)
     (if (nullable? a)
         (re:alt (re:seq (deriv c a) b) (deriv c b))
         (re:seq (deriv c a) b))]
    [(re:star a) (re:seq (deriv c a) (re:star a))]))

(define (re-matches? s r)
  (nullable? (for/fold ([r r]) ([c (in-string s)]) (deriv c r))))

(define (atom c) (if (char=? c #\.) (re:dot) (re:chr c)))

;; parse a LeetCode pattern: literals, '.' = any, 'x*' = zero-or-more
(define (parse pat)
  (let loop ([cs (string->list pat)])
    (match cs
      ['() (re:eps)]
      [(list-rest c #\* rest) (re:seq (re:star (atom c)) (loop rest))]
      [(cons c rest) (re:seq (atom c) (loop rest))])))

(define (is-match s pat) (re-matches? s (parse pat)))

(module+ test
  (require rackunit)
  (check-false (is-match "aa" "a"))
  (check-true  (is-match "aa" "a*"))
  (check-true  (is-match "ab" ".*"))
  (check-true  (is-match "aab" "c*a*b"))
  (check-false (is-match "mississippi" "mis*is*p*."))
  (check-true  (is-match "" "a*")))
