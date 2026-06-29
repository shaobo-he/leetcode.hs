#lang racket/base

;; LeetCode 322: Coin Change — fewest coins summing to `amount`, or #f if
;; impossible.  Bottom-up DP: dp[a] = fewest coins to make `a`.  A vector
;; over 0..amount, each cell filled from the smaller subproblems dp[a - c].
(require racket/vector)

(define (coin-change coins amount)
  (define INF (add1 amount))           ; stands in for "unreachable"
  (define dp (make-vector (add1 amount) INF))
  (vector-set! dp 0 0)
  (for ([a (in-range 1 (add1 amount))])
    (for ([c (in-list coins)])
      (when (and (> c 0) (<= c a))
        (define sub (vector-ref dp (- a c)))
        (when (< (add1 sub) (vector-ref dp a))
          (vector-set! dp a (add1 sub))))))
  (define best (vector-ref dp amount))
  (if (= best INF) #f best))

(module+ test
  (require rackunit)
  (check-equal? (coin-change '(1 2 5) 11) 3)   ; 5 + 5 + 1
  (check-equal? (coin-change '(2) 3) #f)
  (check-equal? (coin-change '(1 2 5) 0) 0)
  (check-equal? (coin-change '(1) 0) 0)
  (check-equal? (coin-change '(1) 2) 2)
  (check-equal? (coin-change '(2 5 10 1) 27) 4)
  (check-equal? (coin-change '(186 419 83 408) 6249) 20))
