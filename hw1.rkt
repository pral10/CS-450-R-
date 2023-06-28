#lang racket
#|
    ===> PLEASE DO NOT DISTRIBUTE THE SOLUTIONS PUBLICLY <===

  We ask that solutions be distributed only locally -- on paper, on a
  password-protected webpage, etc.

  Students are required to adhere to the University Policy on Academic
  Standards and Cheating, to the University Statement on Plagiarism and the
  Documentation of Written Work, and to the Code of Student Conduct as
  delineated in the catalog of Undergraduate Programs. The Code is available
  online at: http://www.umb.edu/life_on_campus/policies/code/

                    * * * ATTENTION! * * *

  Every solution submitted to our grading server is automatically compared
  against a solution database for plagiarism, which includes every solution
  from every student in past semesters.

  WE FOLLOW A ZERO-TOLERANCE POLICY: any student breaking the Code of Student
  Conduct will get an F in this course and will be reported according to
  Section II Academic Dishonesty Procedures.

|#

;; Please, do not remove this line and do not change the function names,
;; otherwise the grader will not work and your submission will get 0 points.
(provide (all-defined-out))

(define ex1 (-
(/ (- 9 10) 6 )
(* 9 (- 14 13))
 ))
             
(define ex2
  (list
   (-
(/ (- 9 10) 6 )
(* 9 (- 14 13))
 )

  (-
  (/ -1 6 )
(* 9 (- 14 13))
 )

  
  (-
  -1/6
(* 9 (- 14 13))
 )

   (-
  -1/6
(* 9 1)
 )

  (-
-1/6
9
 )
    
-55/6
 
     ))


;   (x - y) + (1 + 5) == (2 * y) + y
 (define (ex3 x y)
 (= (+ (- x y) (+ 1 5)) (+ (* 2 y) y) 
   ))

;; Constructs a tree from two trees and a value
(define (tree left value right)
  (list left value right))
 ;(cons left (cons value (cons right null))))
  
  
  
;; Constructs a tree with a single node
(define (tree-leaf value)
(cons null (cons value (cons null empty))))
 
  

;; Accessors
(define (tree-left self)
  (car self))
(define (tree-value self)
  (cadr self))
(define (tree-right self)
  (caddr self))

;; Copies the source and updates one of the fields
(define (tree-set-value self value)
 (tree (first self) value (third self))
)
  
  

  
(define (tree-set-left self left)
 (tree left (second self) (third self))
  )
  
(define (tree-set-right self right)
 (tree (first self) (second self) right)
  )
  


;; Function that inserts a value in a BST

(define (bst-insert self value) 
(cond [(null? self) (tree-leaf value)]
 [(= value (second self)) (tree-set-value self value)]
   [(< value (tree-value self)) (tree-set-left self (bst-insert (tree-left self) value))]
    [else(tree-set-right self (bst-insert (tree-right self) value))]))


;; lambda
(define (lambda? node)
 (and (list? node)  (>= (length node) 3)                    
 (equal? (first node) 'lambda)           
 (list? (second node))
(andmap symbol? (second node))
  )
  )
(define (lambda-params node)
  (second node))

(define (lambda-body node)
 (rest (rest node)))
  

;; apply
(define (apply? l)
( and (list? l)
      (>=(length l) 1)))
  
(define (apply-func node)
  (first node))
(define (apply-args node)
(cdr node)
  )

;; define
(define (define? node)
  (or
   (define-basic? node)
   (define-func? node)
   ))

(define (define-basic? node)
 (and(list? node)
        (>= (length node) 3)
        (equal? (first node) 'define)
        (symbol? (second node))
    ))
(define (define-func? node)
  (and (list? node)
        (>= (length node) 3)
        (equal? (first node) 'define)
        (list? (first (rest node)))
        (> (length (first (rest node))) 0)
        (andmap symbol? (first (rest node)))
        (>= (length (rest (rest node))) 1)
  ))
