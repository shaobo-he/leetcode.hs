#lang racket/base

;; Flood-fill DFS, sinking each island as it's counted. grid = list of strings
;; of #\1 (land) / #\0 (water).
(define (num-islands grid)
  (define rows (length grid))
  (define cols (if (= rows 0) 0 (string-length (car grid))))
  (define cell (make-hash))
  (for ([r (in-range rows)] [row (in-list grid)])
    (for ([c (in-range cols)] [ch (in-string row)])
      (hash-set! cell (cons r c) ch)))
  (define (land? r c) (eqv? (hash-ref cell (cons r c) #\0) #\1))
  (define (sink! r c)
    (when (land? r c)
      (hash-set! cell (cons r c) #\0)
      (sink! (add1 r) c) (sink! (sub1 r) c)
      (sink! r (add1 c)) (sink! r (sub1 c))))
  (for*/sum ([r (in-range rows)] [c (in-range cols)])
    (cond [(land? r c) (sink! r c) 1] [else 0])))

(module+ test
  (require rackunit)
  (check-equal? (num-islands '("11110" "11010" "11000" "00000")) 1)
  (check-equal? (num-islands '("11000" "11000" "00100" "00011")) 3)
  (check-equal? (num-islands '()) 0))
