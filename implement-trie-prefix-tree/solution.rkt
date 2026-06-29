#lang racket/base

;; LeetCode 208: Implement Trie (Prefix Tree).
;;
;; Functional model: a trie node is (cons isEnd children) where children is an
;; assoc list mapping a character to a sub-trie.  Insert/search/startsWith all
;; recurse structurally on the key string (as a list of characters).

(require racket/list)

(define empty-trie (cons #f '()))           ; (isEnd . children)
(define (trie-end? t) (car t))
(define (trie-children t) (cdr t))

;; update the child keyed by ch with (f child-or-empty); first match only
(define (child-update ch f children)
  (cond
    [(null? children) (list (cons ch (f empty-trie)))]
    [(char=? ch (caar children))
     (cons (cons ch (f (cdar children))) (cdr children))]
    [else (cons (car children) (child-update ch f (cdr children)))]))

(define (trie-insert word t)
  (cond
    [(null? word) (cons #t (trie-children t))]
    [else (cons (trie-end? t)
                (child-update (car word)
                              (lambda (sub) (trie-insert (cdr word) sub))
                              (trie-children t)))]))

(define (child-lookup ch children)
  (cond
    [(null? children) #f]
    [(char=? ch (caar children)) (cdar children)]
    [else (child-lookup ch (cdr children))]))

(define (trie-search word t)
  (cond
    [(null? word) (trie-end? t)]
    [else (let ([sub (child-lookup (car word) (trie-children t))])
            (and sub (trie-search (cdr word) sub)))]))

(define (trie-starts-with prefix t)
  (cond
    [(null? prefix) #t]
    [else (let ([sub (child-lookup (car prefix) (trie-children t))])
            (and sub (trie-starts-with (cdr prefix) sub) #t))]))

;; build a trie from a list of words (strings)
(define (build words)
  (foldr (lambda (w acc) (trie-insert (string->list w) acc)) empty-trie words))

(define (search t word) (trie-search (string->list word) t))
(define (starts-with t prefix) (trie-starts-with (string->list prefix) t))

(module+ test
  (require rackunit)
  (define t (build '("apple")))
  (check-equal? (search t "apple") #t)
  (check-equal? (search t "app") #f)
  (check-equal? (starts-with t "app") #t)
  (check-equal? (starts-with t "b") #f)

  ;; a few more, mirroring the classic LeetCode trace
  (define t2 (build '("apple" "app" "application")))
  (check-equal? (search t2 "app") #t)
  (check-equal? (search t2 "appl") #f)
  (check-equal? (starts-with t2 "appl") #t)
  (check-equal? (starts-with t2 "apz") #f)

  ;; refinement spec sanity: search = membership, startsWith = (prefix-of-some)
  (define ws '("cat" "car" "dog"))
  (define t3 (build ws))
  (for ([w ws]) (check-true (search t3 w)))
  (check-equal? (search t3 "ca") #f)
  (check-equal? (starts-with t3 "ca") #t)
  (check-equal? (starts-with t3 "do") #t)
  (check-equal? (starts-with t3 "x") #f))
