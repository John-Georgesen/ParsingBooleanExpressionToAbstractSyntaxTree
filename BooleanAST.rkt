#lang racket
(define (next-token-left-parenthesis?)
  (let ([chr (peek-char)])
    (cond
      [(eqv? chr #\space) (read-char) (next-token-left-parenthesis?)]
      [(eqv? chr #\() #t]
      [else #f]
      )
    )
  )
(define (read-left-parenthesis)
  (read-char)
  )
(define (next-token-right-parenthesis?)
  (let ([chr (peek-char)])
    (cond
      [(eqv? chr #\space) (read-char) (next-token-right-parenthesis?)]
      [(eqv? chr #\)) #t]
      [else #f]
      )
    )
  )
(define (read-right-parenthesis)
  (read-char)
  )
(define (next-token-and?)
  (let ([chr (peek-char)])
    (cond
      [(eqv? chr #\space) (read-char) (next-token-and?)]
      [(eqv? chr #\&) #t]
      [else #f]
      )
    )
  )
(define (read-and)
  (read-char)
  )
(define (next-token-or?)
  (let ([chr (peek-char)])
    (cond
      [(eqv? chr #\space) (read-char) (next-token-or?)]
      [(eqv? chr #\%) #t]
      [else #f]
      )
    )
  )
(define (read-or)
  (read-char)
  )
(define (next-token-not?)
  (let ([chr (peek-char)])
    (cond
      [(eqv? chr #\space) (read-char) (next-token-not?)]
      [(eqv? chr #\!) #t]
      [else #f]
      )
    )
  )
(define (read-not)
  (read-char)
  )
(define (next-token-true?)
  (let ([chr (peek-char)])
    (cond
      [(or (eqv? chr #\newline) (eqv? chr #\space)) (read-char) (next-token-true?)]
      [else
       (let ([str (peek-string 4 0)])
         (cond
           [(= (string-length str) 5)
            (if (and (equal? (substring str 0 4) "true") (char-whitespace? (string-ref str 4))) #t #f)]
            [(equal? str "true") #t]
            [else #f]
           )
         )
       ]
      )
    )
  )
(define (read-true)
  (read-string 4)
  )

(define (next-token-false?)
  (let ([chr (peek-char)])
    (cond
      [(or (eqv? chr #\newline) (eqv? chr #\space)) (read-char) (next-token-false?)]
      [else
       (let ([str (peek-string 5 0)])
         (cond
           [(= (string-length str) 6)
            (if (and (equal? (substring str 0 5) "false") (char-whitespace? (string-ref str 5))) #t #f)]
            [(equal? str "false") #t]
            [else #f]
           )
         )
       ]
      )
    )
  )
(define (read-false)
  (read-string 5)
  )
(define (read-identifier)
  (read-char)
  )
(define (next-token-end?)
  (let ([chr (peek-char)])
    (cond
      [(eqv? chr #\space) (read-char) (next-token-end?)]
      [(eqv? chr #\newline) #t]
      [else #f]
      )
    )
  )
(define (retrieve-value varname var-list)
  (cond
    [(eqv? var-list '())
     (display ("variable not declared"))]
    [(hash-has-key? (car var-list) varname) (hash-ref (car var-list) varname)]
    [else (retrieve-value varname (cdr var-list))]
    )
  )
      

(define (translate-real-expr)
  (let ([real-term (translate-real-term)])
    (cond
      [(next-token-or?) (read-or) (cons real-term (cons #\% (cons (translate-real-expr) '())))]
      [else (cons real-term '())]
      )
    )
  )

(define (translate-real-term)
  (let ([real-factor (translate-real-factor)])
    (cond
       [(next-token-and?) (read-and) (cons real-factor (cons #\& (cons (translate-real-term) '())))]
       [else (cons real-factor '())]
      )
    )
  )

(define (translate-real-factor)
  (cond
    [(next-token-left-parenthesis?) (read-left-parenthesis)
                                    (let ([real-expr (translate-real-expr)])
                                      (cond
                                        [(next-token-right-parenthesis?) (read-right-parenthesis) (cons real-expr '())]
                                        [else (display "Unpaired parenthesis detected.\n")]
                                        )
                                      )
                                    ]
    [(next-token-not?) (read-not) (cons #\! (translate-real-factor))]
    [(next-token-true?) (read-true)(cons #t '())]
    [(next-token-false?) (read-false) (cons #f '())]
    [else (cons (read-identifier) '())]
    )
  )

(define (eval-real-expr real-ast var-list)
  (let ([val1 (eval-real-term (car real-ast) var-list)])
    (cond
      [(= 1 (length real-ast)) val1]
      [(eqv? #\% (cadr real-ast)) (or val1 (eval-real-expr (caddr real-ast) var-list))]
      [else val1]
      )
    )
  )
(define (eval-real-term real-ast var-list)
  (let ([val1 (eval-real-factor (car real-ast) var-list)])
    (cond
      [(= 1 (length real-ast)) val1]
      [(eqv? #\& (cadr real-ast)) (and val1 (eval-real-term (caddr real-ast) var-list))]
      [else val1]
      )
    )
  )

(define (eval-real-factor real-ast var-list)
  (cond
    [(list? (car real-ast)) (eval-real-expr (car real-ast) var-list)]
    [(eqv? #\! (car real-ast)) (not (eval-real-factor (cdr real-ast) var-list))]
    [(boolean? (car real-ast)) (car real-ast)]
    [else (let ([val (retrieve-value (car real-ast) var-list)])
            (cond
              [(boolean? val) val]
              [else (display "Not real var")]
              ))]
    )
  )

(define var-table (make-hash))
(hash-set! var-table 'x 5)
(hash-set! var-table 'y 4)

(define real-ast (translate-real-expr))
(display real-ast)
(newline)
(eval-real-expr real-ast (cons var-table '()))
