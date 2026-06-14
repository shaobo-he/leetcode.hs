#lang racket

;; Course Schedule II (LeetCode 210): return one order in which every course
;; can be taken, or '() if the prerequisites contain a cycle.
;; Each prerequisite (list a b) means course b must be taken before course a.
(define/contract (find-order num-courses prerequisites)
  (-> exact-integer? (listof (listof exact-integer?)) (listof exact-integer?))
  ;; adjacency: prereq -> courses that depend on it
  (define graph
    (foldl (lambda (edge g)
             (define course (car edge))
             (define prereq (cadr edge))
             (hash-update g prereq (lambda (succ) (cons course succ)) '()))
           (hash)
           prerequisites))
  (define state (make-hash))    ;; node -> 'visiting | 'done
  (define order '())            ;; topological order, prepended in post-order
  (define ok? #t)
  (define (visit node)
    (case (hash-ref state node 'unseen)
      [(done) (void)]
      [(visiting) (set! ok? #f)]                ;; back-edge => cycle
      [else
       (hash-set! state node 'visiting)
       (for ([next (in-list (hash-ref graph node '()))])
         (visit next))
       (hash-set! state node 'done)
       (set! order (cons node order))]))
  (for ([course (in-range num-courses)] #:when ok?)
    (visit course))
  (if ok? order '()))

(module+ test
  (require rackunit)
  ;; a result is valid when it lists every course once and respects prereqs
  (define (valid-order? n prereqs order)
    (and (equal? (sort order <) (range n))
         (for/and ([p (in-list prereqs)])
           (< (index-of order (cadr p)) (index-of order (car p))))))
  (define dag '((1 0) (2 0) (3 1) (3 2)))
  (check-true  (valid-order? 4 dag (find-order 4 dag)))
  (check-true  (valid-order? 2 '() (find-order 2 '())))
  (check-equal? (find-order 2 '((1 0) (0 1))) '()))   ;; cycle
