#lang racket

;; Recursive-descent check using an escape continuation: parse consumes a
;; balanced prefix and returns the rest; any mismatch escapes with #f.
(define/contract (is-valid s)
  (-> string? boolean?)
  (let/cc k
    (define (parse lst)
      (define (matched-parse rp rst)
        (define rst^ (parse rst))
        (match rst^
          [(cons rp^ rst^^)
           #:when (equal? rp rp^)
           (parse rst^^)]
          [_ (k #f)]))
      (match lst
        [(cons #\( rst) (matched-parse #\) rst)]
        [(cons #\[ rst) (matched-parse #\] rst)]
        [(cons #\{ rst) (matched-parse #\} rst)]
        [_ lst]))
    (null? (parse (string->list s)))))

(module+ test
  (require rackunit)
  (check-true  (is-valid "()[]{}"))
  (check-false (is-valid "(]"))
  (check-true  (is-valid "{[]}"))
  (check-false (is-valid "("))
  (check-false (is-valid ")("))
  (check-true  (is-valid "")))
