(define (succs state)
  (define cnt (car state))
  (define perm (cdr state))
  (map
   (λ (k)
     (cons
      (if (= (hash-ref cnt k) 1)
          (hash-remove cnt k)
          (hash-update cnt k sub1))
      (cons k perm)))
     (hash-keys cnt)))

(define (end? state)
  (hash-empty? (car state)))

(define (search states)
  (cond
    [(null? states) '()]
    [else
     (define state (car states))
     (if (end? state)
         (cons state (search (cdr states)))
         (search (append (succs state) (cdr states))))]))

(define (count xs)
  (foldl
   (λ (x cnt)
     (if
      (hash-has-key? cnt x)
      (hash-update cnt x add1)
      (hash-set cnt x 1)))
   (make-immutable-hash)
   xs))

(define (extract-results states)
  (map cdr states))

(define/contract (permute-unique nums)
  (-> (listof exact-integer?) (listof (listof exact-integer?)))
  (define cnt (count nums))
  (define init-states (list (cons cnt '())))
  (extract-results (search init-states)))
