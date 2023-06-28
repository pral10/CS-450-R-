#lang racket
#|
    ===> PLEASE DO NOT DISTRIBUTE SOLUTIONS NOR TESTS PUBLICLY <===

   We ask that solutions be distributed only locally -- on paper, on a
   password-protected webpage, etc.

   Students are required to adhere to the University Policy on Academic
   Standards and Cheating, to the University Statement on Plagiarism and the
   Documentation of Written Work, and to the Code of Student Conduct as
   delineated in the catalog of Undergraduate Programs. The Code is available
   online at: http://www.umb.edu/life_on_campus/policies/code/

|#
(require rackunit)
(require "ast.rkt")
(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;
;; Utility functions ;;

(define p:empty (delay empty))
(define (p:empty? p) (empty? (force p)))
(define (p:first l) (car (force l)))
(define (p:rest l) (cdr (force l)))
(define (stream-get stream) (car stream))
(define (stream-next stream) ((cdr stream)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Exercise 1
(define p:void
  (delay empty) ;promise holding empty list
)
;; Exercise 2
(define p:epsilon
 ( delay (cons (quote "") (delay empty)))) ; empty promise list holding empty string

;; Exercise 3
(define (p:char p)
  (delay (cons (string p) (delay empty)))) ; converting set of string with the single character

;; Exercise 4
(define (p:union p1 p2)
  (cond [(p:empty? p1) p2] ; if p1 and p2 is empty delay empty that is create empty promise list
  [else (delay (cons (p:first p1) (delay (force (p:union p2 (p:rest p1))))))])) ;else  find union of p:first with recursive call where p2 is first agrument and rest of p1 is second


;; Exercise 5
(define (p:prefix s p)
  (cond [(p:empty? p) (delay empty)] ; if the p is empty delay empty
  [else (delay (cons   
  (string-append s (p:first p)) ; else we concatinate the every string of s on every element of promise list p.
  (delay (force (force(p:prefix s (p:rest p)))))))]))

;; Exercise 6
(define (p:cat p1 p2) 'todo)

;; Exercise 7
(define (p:star union pow p)
  (define (aux num)
   (delay (force (union (pow p num) (delay (force (aux (+ num 1))))))))
  (aux 0)
  )

;; Exercse 8
(define (stream-foldl f a s)
(cond ((null? s) a)  ; define next with the argument f streat-get the utility function given and a
 (else
 (cons a
  (thunk (stream-foldl f (f (stream-get s) a) (stream-next s))))))) ; calling f a with the thunk so we can create function with zero argument and calculate when needed calling the recurisve stream-fold with required arguments.


;; Exercise 9
(define (stream-skip n s)
   (cond [(equal? n 0) s] ; if the n is equals to 0 return s
        [(< n 0) s]  ; if n is less than 0 also returns s
        [else (stream-skip (- n 1) (stream-next s))])) ; else recursive call the stream-skip with every time decreasing value one and stream-next with s
                                                        ;where it is utility function to get cdr of stream
;; Exercise 10
(struct r:bool (value) #:transparent) ; implementing data structure r:bool using struct with value which is transparent 
(define (and-procedure)
    (lambda inp 
       (cond[(empty? inp) #t]; if the lamda input is empty return true
       [else (foldl (lambda (first second) (and second first)) (car inp) inp)] 
    )
   )
)

(define (r:eval-builtin sym)
  (cond [(equal? sym '+) +]
        [(equal? sym '*) *]
        [(equal? sym '-) -]
        [(equal? sym '/) /]
        [(equal? sym 'and) (and-procedure)]
        [else #f]))

(define (r:eval-exp exp)
  (cond
    ;1.When evaluating a number, just return that number
    [(r:number? exp) (r:number-value exp)]
    ; 2. When evaluating an arithmetic symbol,
    ; return the respective arithmetic function
    [(r:variable? exp) (r:eval-builtin (r:variable-name exp))]
    ; 3. When evaluating a function call evaluate each expression and apply
    ; the first expression to remaining ones

  [(r:apply? exp) ; r:apply expression
  (apply (r:eval-exp (r:apply-func exp)) ; apply r:eval-exp (r:apply-function exp)
    (cond [(list? (r:apply-args exp)) ; if the r:apply-args ecp is list
    (map r:eval-exp (r:apply-args exp)) ; map r:eval-exp with r:apply-args exp
    ]
    [else 
    (r:eval-exp (first (r:apply-args exp))) ;else condition which was given
    (r:eval-exp (second (r:apply-args exp)))]))
  ]
 [(r:bool? exp) (r:bool-value exp)] ; if r:bool is exp call (r:bool-value exp)
     [else (error "Unknown expression:" exp)]))
