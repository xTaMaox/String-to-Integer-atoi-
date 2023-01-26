(require (for-syntax racket/match))
(define-syntax (make-parser stx)
  (match (syntax->list stx)
    [(list _ match-expr match-expr-fn other-expr-fn)
     (datum->syntax
      stx
      `(lambda (ls)
         (if (empty? ls) '()
             (let ([fs (first ls)])
               (match fs
                 [,match-expr ,match-expr-fn]
                 [_ ,other-expr-fn])))))]))

(define parse-spaces (make-parser #\space (parse-spaces (rest ls)) (parse-sign ls)))

(define parse-sign (make-parser (? (lambda (c) (or (equal? c #\+) (equal? c #\-))))
                                (append (list fs) (parse-numbers (rest ls)))
                                (parse-numbers ls)))

(define parse-numbers (make-parser (? char-numeric?) (append (list fs) (parse-numbers (rest ls))) '()))

(define (parse ls) (parse-spaces ls))

(define (adj-string->number s)
  (let ([attempt (string->number s)])
    (if attempt attempt 0)))

(define (first-token s)
  (let ([split (string-split s)])
    (if (empty? split)
        "0"
        (first split))))

(define (32-bit-range-clip i)
  (let ([lower (- (expt 2 31))] [upper (sub1 (expt 2 31))])
    (cond [(<= lower i upper) i]
          [(< i lower) lower]
          [else upper])))

(define (my-atoi s)
  (let ([strls (string->list (first-token s))])
    (32-bit-range-clip (adj-string->number (list->string (parse strls))))))