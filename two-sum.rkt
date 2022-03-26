#lang typed/racket/base

(: two-sum (-> (Vectorof Integer) Integer (Pairof Integer Integer)))
(define (two-sum lst sum)
  (: len Integer)
  (define len (vector-length lst))
  (define map (make-hash
               (build-list
                len
                (λ ([idx : Integer]) (cons (vector-ref lst idx) idx)))))
  (: search
     (-> Integer
         (U (Pairof Integer Integer) False)
         (U (Pairof Integer Integer) False)))
  (define (search idx p)
    (define elem (vector-ref lst idx))
    (cond
      [(and (equal? p #f)
            (hash-has-key? map (- sum elem)))
       (cons idx (hash-ref map (- sum elem)))]
      [else p]))
  (let ([result (foldl search #f (build-list len (λ ([x : Integer]) x)))])
    (if (equal? result #f)
        (error "not such pair!")
        result)))
