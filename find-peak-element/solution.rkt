#lang racket/base

;; LeetCode 162: Find Peak Element.
;; Binary search for a peak: compare v[mid] with v[mid+1] and recurse into the
;; ascending side (keeping mid on the left branch).  Returns an index i with
;; v[i] greater than each of its in-bounds neighbours, in O(log n) time.  The
;; search is justified by an INVARIANT, not by the array being sorted (see the
;; Lean proof in solution.lean).
(define (find-peak xs)
  (define v (list->vector xs))
  (define n (vector-length v))
  (let go ([lo 0] [hi (sub1 n)])
    (cond
      [(>= lo hi) lo]
      [else
       (define mid (quotient (+ lo hi) 2))
       (if (< (vector-ref v mid) (vector-ref v (add1 mid)))
           (go (add1 mid) hi)
           (go lo mid))])))

;; PeakAt: boundaries handled as disjunctions (no -inf sentinel).
(define (peak-at? xs i)
  (define v (list->vector xs))
  (define n (vector-length v))
  (and (or (= i 0)        (< (vector-ref v (sub1 i)) (vector-ref v i)))
       (or (= (add1 i) n) (< (vector-ref v (add1 i)) (vector-ref v i)))))

(module+ test
  (require rackunit)
  (check-equal? (find-peak '(1 2 3 1)) 2)
  (check-true   (peak-at? '(1 2 3 1) (find-peak '(1 2 3 1))))
  ;; [1,2,1,3,5,6,4] has two peaks (1 and 5); the search lands on one of them.
  (check-true   (peak-at? '(1 2 1 3 5 6 4) (find-peak '(1 2 1 3 5 6 4))))
  (check-equal? (find-peak '(1)) 0)
  (check-true   (peak-at? '(1) (find-peak '(1))))
  (check-true   (peak-at? '(1 2) (find-peak '(1 2))))
  (check-true   (peak-at? '(2 1) (find-peak '(2 1)))))
