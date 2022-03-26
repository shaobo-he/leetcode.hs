#lang racket

(define/contract (find-order numCourses prerequisites)
  (-> exact-integer? (listof (listof exact-integer?)) (listof exact-integer?))
  (define graph (foldl (λ (t r) (hash-set r (cadr t) (car t))) (hash) prerequisites))
  (define (dfs course)
  
  '()
  )