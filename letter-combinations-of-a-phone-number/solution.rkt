#lang racket/base

(define digit->letters
  (hash #\2 '(#\a #\b #\c)
        #\3 '(#\d #\e #\f)
        #\4 '(#\g #\h #\i)
        #\5 '(#\j #\k #\l)
        #\6 '(#\m #\n #\o)
        #\7 '(#\p #\q #\r #\s)
        #\8 '(#\t #\u #\v)
        #\9 '(#\w #\x #\y #\z)))

;; Cartesian product of the letter sets for each digit.
(define (letter-combinations digits)
  (cond
    [(= (string-length digits) 0) '()]
    [else
     (map list->string
          (foldr (lambda (d acc)
                   (for*/list ([l (in-list (hash-ref digit->letters d))]
                               [rest (in-list acc)])
                     (cons l rest)))
                 '(())
                 (string->list digits)))]))

(module+ test
  (require rackunit racket/set)
  (check-equal? (list->set (letter-combinations "23"))
                (list->set '("ad" "ae" "af" "bd" "be" "bf" "cd" "ce" "cf")))
  (check-equal? (letter-combinations "") '())
  (check-equal? (length (letter-combinations "7")) 4))
