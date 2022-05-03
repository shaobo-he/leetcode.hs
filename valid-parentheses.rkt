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
