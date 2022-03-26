#lang racket/base

(require racket/contract)
(require math/array)

(define/contract (is-valid-sudoku board)
  (-> (listof (listof char?)) boolean?)

  )
