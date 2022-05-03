(require parser-tools/lex)
(require (prefix-in : parser-tools/lex-sre))
;(require racket/match)

(define sa-lex
  (lexer
   [whitespace (sa-lex input-port)]
   [(:+ (char-range #\0 #\9)) (cons `(NUM ,(string->number lexeme)) (sa-lex input-port))]
   [#\( (cons `(LPAR) (sa-lex input-port))]
   [#\) (cons `(RPAR) (sa-lex input-port))]
   [(union #\+ #\- #\* #\/) (cons `(OP ,(string->symbol lexeme)) (sa-lex input-port))]
   [(eof) '()]))

;; expr := prod {{+|-} prod}*
;; prod := atom {{*|/} atom}*
;; atom := (expr)
;;      |  num
;;      |  -atom

(define (sa-cal str)
  (define div
    (λ (x y)
      (if
       (>= (* x y) 0)
       (floor (/ x y))
       (ceiling (/ x y)))))
  (define (cl/+- op v i)
    (match (sa-prod i)
      [(cons a (cons '(OP +) i1))
       (cl/+- + (op v a) i1)]
      [(cons a (cons '(OP -) i1))
       (cl/+- - (op v a) i1)]
      [t (cons (op v (car t)) (cdr t))]))
  (define (cl/*/ op v i)
    (match (sa-atom i)
      [(cons a (cons '(OP *) i1))
       (cl/*/ * (op v a) i1)]
      [(cons a (cons '(OP /) i1))
       (cl/*/ div (op v a) i1)]
      [t (cons (op v (car t)) (cdr t))]))
  (define (sa-ex i)
    (match (sa-prod i)
      [(cons a (cons '(OP +) i1))
       (cl/+- + a i1)]
      [(cons a (cons '(OP -) i1))
       (cl/+- - a i1)]
      [t t]))
  (define (sa-prod i)
    (match (sa-atom i)
      [(cons a (cons '(OP *) i1))
       (cl/*/ * a i1)]
      [(cons a (cons '(OP /) i1))
       (cl/*/ div a i1)]
      [t t]))
  (define (sa-atom i)
    (match i
      [(cons '(LPAR) i1)
       (match (sa-ex i1)
         [(cons a (cons '(RPAR) r)) (cons a r)]
         [_ (error "unmatched par")])]
      [(cons (list 'NUM n) r) (cons n r)]
      [(cons '(OP -) i1)
       (let ([r (sa-atom i1)])
         (cons (- 0 (car r)) (cdr r)))]))
  (sa-ex (sa-lex (open-input-string str))))
      
(define/contract (calculate s)
  (-> string? exact-integer?)
(car (sa-cal s))
  )
