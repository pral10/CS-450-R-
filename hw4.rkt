#|
    ===> PLEASE DO NOT DISTRIBUTE THE SOLUTIONS PUBLICLY <===

   We ask that solutions be distributed only locally -- on paper, on a
   password-protected webpage, etc.

   Students are required to adhere to the University Policy on Academic
   Standards and Cheating, to the University Statement on Plagiarism and the
   Documentation of Written Work, and to the Code of Student Conduct as
   delineated in the catalog of Undergraduate Programs. The Code is available
   online at: http://www.umb.edu/life_on_campus/policies/code/

|#
;; PLEASE DO NOT CHANGE THE FOLLOWING LINES
#lang racket
(provide (all-defined-out))
(require "hw4-util.rkt")
;; END OF REQUIRES

;; Utility functions
(define (s:apply-arg1 app)
  (first (s:apply-args app)))
(define (s:lambda-param1 lam)
  (first (s:lambda-params lam)))
(define (s:lambda-body1 lam)
  (first (s:lambda-body lam)))
;; Utility functions
(define (e:apply-arg1 app)
  (first (e:apply-args app)))
(define (e:lambda-param1 lam)
  (first (e:lambda-params lam)))
(define (e:lambda-body1 lam)
  (first (e:lambda-body lam)))

;; Exercise 1
(define (s:subst exp var val)
 (cond[(s:expression? exp) 
  (cond 
   [(s:number? exp) exp]
   [(s:variable? exp)
    (cond
      [(equal? exp var) val]
      [else exp])]
   [(s:value? exp)
    (cond[(equal? (s:lambda-param1 exp) var) exp]
                  [ else (s:lambda (list (s:lambda-param1 exp)) (list (s:subst (s:lambda-body1 exp) var val)))])]
   
   [else (s:apply(s:subst(s:apply-func exp) var val)
             (list(s:subst(s:apply-arg1 exp) var val)))])]))


;; Exercise 2
(define (s:eval subst exp)
  (cond [(s:value? exp) exp]
        [(s:apply? exp)
         (s:eval subst (s:subst (s:lambda-body1 (s:eval subst (s:apply-func exp)))
                                (s:lambda-param1 (s:eval subst (s:apply-func exp)))
                                (s:eval subst (s:apply-arg1 exp))))]))
                                                    
;; Exercise 3
(define (e:eval env exp)
  (cond[(e:expression? exp)

  (define h (hash))
  (cond
    [(e:value? exp) exp]
    [(and (e:variable? exp) (hash? env) (hash-has-key? env exp))(hash-ref env exp)] 
    [(and (e:lambda? exp) (hash? env)) (e:closure env exp)]
    [(e:apply? exp)
     (define eb (e:lambda-body1 (e:closure-decl (e:eval env (e:apply-func exp)))))
     (define env1 (hash-set (e:closure-env (e:eval env (e:apply-func exp)))
                            (e:lambda-param1 (e:closure-decl (e:eval env (e:apply-func exp))))
                            (e:eval env (e:apply-arg1 exp))))
     (e:eval env1  (e:lambda-body1 (e:closure-decl (e:eval env (e:apply-func exp)))))
             ])]
     [else 
     (error "Unknown expression:" exp)
     ]
     )
)

       
;; Exercise 4 (Manually graded)
#|
  The reason when implementing the lambda racket without environment is better than with evironment becasue when we run the lambda racket without the environment
it will end up accessing the less memory as no environment variable and procedure will be presented.
The search time would be less as in such situation we only have a few amount of data and the function evaluation won't be looking into all the environment for subsitute depending
as it works within the variable and symbols only available which make it straight forward. For example working on the few pair of data as we can easily work on those pair without building
the hash table.

conversly implementing the lamda racket with environment is better because it helps us to bind value with the symbol. We can look up the symbols and defination easily as it is already presented
within the environment. It will take less time however we have to use the more memory space. For example the hash table , if we have large hash table with huge pairs here using table we can have
better search function.
|#

;; Exercise 5 (Manually graded)
#|
 Two benifits of using a formal specification to help with the implementation of software system are:
1) It can help developer to understand algorithms straight as the syntax, expression are well defined formally that helps to understand the design of the program
and help in debugging and understanding the programs as they formally tend to highlight any completeness.
2) It can help save the space and also make it easier for software engineer to work and understand each other work easily as well as quick time to fix the issue they run into and help in providing
insight and understanding of software requirements by verifying the correctness of design by using proofs to validate specification. 
|#
