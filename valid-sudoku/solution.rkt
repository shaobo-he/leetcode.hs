#lang racket/base

(require racket/contract
         racket/list)

;; A 9x9 board is valid when no row, column, or 3x3 sub-box contains a
;; repeated digit (1-9). #\. marks an empty cell and is ignored.
(define/contract (is-valid-sudoku board)
  (-> (listof (listof char?)) boolean?)
  (define (no-dups? cells)
    (define digits (filter (lambda (c) (not (char=? c #\.))) cells))
    (= (length digits) (length (remove-duplicates digits))))
  (define rows board)
  (define cols (apply map list board))            ;; transpose
  (define boxes
    (for*/list ([box-row (in-range 0 9 3)]
                [box-col (in-range 0 9 3)])
      (for*/list ([r (in-range box-row (+ box-row 3))]
                  [c (in-range box-col (+ box-col 3))])
        (list-ref (list-ref board r) c))))
  (andmap no-dups? (append rows cols boxes)))

(module+ test
  (require rackunit)
  (define (board . rows) (map string->list rows))
  ;; LeetCode example 1 -> valid
  (check-true
   (is-valid-sudoku
    (board "53..7...." "6..195..." ".98....6." "8...6...3"
           "4..8.3..1" "7...2...6" ".6....28." "...419..5" "....8..79")))
  ;; example 2: leading 8 collides within its column and box -> invalid
  (check-false
   (is-valid-sudoku
    (board "83..7...." "6..195..." ".98....6." "8...6...3"
           "4..8.3..1" "7...2...6" ".6....28." "...419..5" "....8..79"))))
