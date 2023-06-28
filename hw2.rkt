#lang racket
#|
            #####################################################
            ###  PLEASE DO NOT DISTRIBUTE SOLUTIONS PUBLICLY  ###
            #####################################################

  Copy your solution of HW1 as file "hw1.rkt". The file should be in the same
  directory as "hw2.rkt" and "ast.rkt".
|#
(require "ast.rkt")
(require "hw1.rkt")
(require rackunit)
(provide (all-defined-out))
;; ^^^^^ DO NOT CHANGE ANY CODE ABOVE THIS LINE ^^^^^


;; Exercise 1.a: Counter
(define (counter accum grow)
   (lambda (input)      ;  lambda with input for symbol
    (cond 
          [(equal? 'get input) accum]    ; when lambda get is equal to input return accumlator
          [(equal? 'inc input) (counter (grow accum) grow)]  ; when lambda inc is equal to input  recursice call counter with (grow accum)  grow)
          [else (void)])))  ; else returns void

;; Exercise 1.b: Adder
(define (adder super)
  (lambda (input) ; lamda with input for symbol
    (cond 
          [(equal? 'get input) (super 'get)]   ; when lamda get is equal to input returns the accumulated value for super
          [(equal? 'inc input) (adder ((super 'inc) 'inc))] ; when symbol is lamda inc return new adder with an internal state incrementing super twice
          [else (void)]))) ; else returns void

;; Exercise 2: Interperse
(define (intersperse l v)
  (cond
      [(empty? l) l] ; if the list is empty returns list
      [(empty? (rest l)) l] ; if the rest of the l is empty returns list
      [else (cons (first l) ; else returns the list where we add element e beyween each pair of of element in l implementing tail recursion
                  (cons v
                        (intersperse (rest l) v)))]))
                        
;; Exercise 3.a: Generic find
(define (find pred l)
   (define (inside pred l index) ; defines inside with pred l and index
    (cond
      [(empty? l) #f]   ; if the list is empty returns the boolen false 
      [(pred index (first l)) (cons index (first l))] ; returns the index value if it's found
      [else (inside pred (rest l) (+ index 1))] ; else not found not empty keet calling the recursive to find with index value increasing and looking for rest list value
    ))
  (inside pred l 0)) ;index is set to 0 


;; Exercise 3.b: Member using find
(define (member x l)
    (cond
    [(equal? (find (lambda (idex elem) (equal? elem x)) l) #f) #f] ;implementing in term of find function returns false if x is not found in the list l 
    [else #t] ; else return true i did it opposite to what being asked returned true else wise
  ))

;; Exercise 3.c: index-of using find
(define (index-of l x)
  (cond
    [(member x l) (car (find (lambda (idex elem) (equal? elem x)) l))] ; implemented index-of in terms of find index-of takes a list l and an
                                                                          ;element x and returns the index of the first occurrence of element x in list l
    [else #f] ;else return false
  ))

;; Exercise 4: uncurry
(define (uncurry f)
  (define (open f l) ; define function 
    (cond
      [(equal? l (list)) (f)] ; when l equal list return (f)
      [(equal? 1 (length l)) (f (first l))]   ; when length of l is equal to 1 return f with first of l
      [else (open (f (first l)) (rest l) )])) ; else recursively call open f with first l and rest of l
  (lambda (l) (open f l))) 

;; Exercise 5: Parse a quoted AST
(define (parse-ast node)
(define (make-define-func node)
    (r:define         
    (parse-ast (first (second node)))  
       (r:lambda                       
      (map parse-ast (rest (second node)))
      (map parse-ast (rest (rest node)) ) 
    )
  ))

  (define (make-define-basic node)
    (r:define                     
    (parse-ast (second node))
    (parse-ast (third node))
  ))

  (define (make-lambda node)

    (r:lambda            
    (map parse-ast (second node))
    (map parse-ast (rest (rest node)))
  ))

  (define (make-apply node)
    (r:apply   
    (parse-ast (first node))
    (map parse-ast (rest node))
  ))

  (define (make-number node)
    (r:number node))

  (define (make-variable node)
    (r:variable node))

  (cond
    [(define-basic? node) (make-define-basic node)]
    [(define-func? node) (make-define-func node)]
    [(symbol? node) (make-variable node)]
    [(real? node) (make-number node)]
    [(lambda? node) (make-lambda node)]
    [else (make-apply node)]))