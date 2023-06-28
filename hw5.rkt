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
(require "hw5-util.rkt")
(require rackunit)
(provide d:eval-exp d:eval-term)
(define (d:apply-arg1 app)
  (first (d:apply-args app)))
(define (d:lambda-param1 lam)
  (first (d:lambda-params lam)))
;; END OF REQUIRES

;; Exercise 1

(define/contract (d:eval-exp mem env exp)
  (-> mem? handle? d:expression? eff?)
        (cond  
            [(d:value? exp) (eff mem exp)]
            [(d:variable? exp) (eff mem (environ-get mem env exp))]
            [(d:lambda? exp)
             (eff mem (d:closure env exp))]             
             [(d:apply? exp)
                (define ef (d:apply-func exp))  
                (define Ef-closure (d:eval-exp mem env ef))
                (define memm (eff-state Ef-closure))
                (define ea (d:eval-exp memm env (d:apply-arg1 exp)))
                (define closure (eff-result Ef-closure))
                (define Ef (d:closure-env closure))
                (define lmda (d:closure-decl closure))
                (define x (d:lambda-param1 lmda))
                (define tb (d:lambda-body lmda))
                (define emb (environ-push (eff-state ea) Ef x (eff-result ea)))
                (define vb (d:eval-term (eff-state emb) (eff-result emb) tb))
                (eff (eff-state vb) (eff-result vb))
             ]
             
             [else (exp)]
            )     
  )

;; Exercise 2
(define/contract (d:eval-term mem env term)
  (-> mem? handle? d:term? eff?)
    (cond
      [(d:expression? term) (d:eval-exp mem env term)]
      [(d:define? term)
              (define v (d:eval-term mem env (d:define-body term)))
              (define new (environ-put (eff-state v) env (d:define-var term) (eff-result v)))
              (d:eval-term new env (d:void))
      ]
     [(d:seq? term)
     (define sr (d:eval-term mem env (d:seq-fst term)))
     (define memm (eff-state sr))
     (define envv (eff-result sr))
     (define sr2 (d:eval-term memm env (d:seq-snd term)))
     (eff (eff-state sr2) (eff-result sr2))]
  )
)

;; Exercise 3 (Manually graded)
#|

As we know that λD is dynamically scoped, while the Racket is lexically scoped.
In Lambda racket, easier binding will have defination hold same value throughout the program
unless it is changed but in normal racket as there is no environment and different binding makes
the value of the variable to be defined manually over and over again in other function to get the
output as it won't be updated.
 Example of the simple program is:
(define (x 10)
(define (f (lambda (y) (+ x y))
(define (x 0) 
(f 10)
)))
In Racket, it is evaluated to 20.
In λD, it is evaluated to 10.

|#
