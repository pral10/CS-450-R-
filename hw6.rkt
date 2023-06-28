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

|#
;; PLEASE DO NOT CHANGE THE FOLLOWING LINES
(require racket/set)
(require "hw6-util.rkt")
(provide frame-refs mem-mark mem-sweep mlist mapply)
;; END OF REQUIRES

;;;;;;;;;;;;;;;
;; Exercise 1
;
; Given a frame, return all handles contained therein.
;
; Use frame-fold and set-add; only closure's env is relevant for this problem

(define/contract (frame-refs frm)
  (-> frame? set?)
  (define (proc l)
    (cond
      [(empty? l) (set)]
      [(d:closure? (first l))
       (cond[(not (empty? l))
       (set-add (proc (rest l)) (d:closure-env (first l)))])
         ]
         [else (set-add (proc (rest l)) (set))
         ]
     )
     )
  (cond[(frame-parent frm)
        (set-subtract (set-add (proc (frame-values frm)) (frame-parent frm)) (set (set))) ]
    [else (set-subtract (proc (frame-values frm)) (set (set)))]
    )
  )

;;;;;;;;;;;;;;;
;; Exercise 2
; Standard graph algorithm: return all handles reacheable via `contained`.
; Hint: This is a simple breadth-first algorithm. The algorithm should start
;       in env and obtain the set of next elemens by using `contained`.
; Hint: The algorithm must handle cycles.
; Hint: Do not hardcode your solution to frames (you should test it with
;       frames though)

(define/contract (mem-mark contained mem env)
  (-> (-> any/c set?) heap? handle? set?)
(define (mem-mark-helper env)
        (cond 
        [(not (frame-parent (heap-get mem env))) (set env)]
        [(or (equal? (frame-parent (heap-get mem env)) env)) (contained (heap-get mem env))]
         [else
          (set-add (set-union (contained (heap-get mem env)) (mem-mark-helper (frame-parent (heap-get mem env)))) env)
         ]
         )
     )
     (mem-mark-helper env)
)

;;;;;;;;;;;;;;;
;; Exercise 3
;
; Return a new heap that only contains the key-values referenced in to-keep.
;
; Tip 1: We have learned a similar pattern than what is asked here.
; Tip 2: The function you want to use starts with heap-
; Tip 3: The solution is a one-liner

(define/contract (mem-sweep mem to-keep)
  (-> heap? set? heap?)
  (heap-filter (lambda (key value) (set-member? to-keep key)) mem))


;;;;;;;;;;;;;;;
;; Exercise 4

; Recursively evaluate each argument and use bind to sequence the recursion step
;
; x1 <- op1
; x2 <- op2
; x3 <- op3
;
; Should become:
;
; l <- (list op1 op2 op3)
;
; Tip: The solution of this example is quite similar to the example in
;      yield to-list (video 6 of lecture 32)

(define (mlist bind pure args)
  (define (list-helper accum listt)
    (cond [(empty? listt) (pure accum)]
          [else
           (bind (first listt) (lambda(l) (list-helper (append accum (list l)) (rest listt))))]
  ))
  (list-helper (list) args)
  )

;;;;;;;;;;;;;;;
;; Exercise 5

; Can be solved with one line if you use mlist, apply, and compose.
;
(define (mapply bind pure f . args)
  (mlist bind (lambda (arg) (pure (apply f arg))) args))

;;;;;;;;;;;;;;;
;; Exercise 6 (MANUALLY GRADED)
#|
when the reference count algorithm is faulty, the reference count may affect soundness as the memory could be allocated at a different place before
due to the resetting the counter to zero can execute a different reference to memory. If it occurs, it would affect the explicity of the results
since we are reclaiming memory too early due to the overflow. Also, it could not affect the completeness of memory management since the memory 
is being affected.
|#
