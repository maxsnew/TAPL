#lang racket
(require redex
         "TypedBooleans.rkt"
         "../3/Naturals.rkt")

;; Figure 8-2
(define-union-language N+Bt
  NB Bt)

(define-extended-language NBt
  N+Bt
  (T ....
     Nat))

(define-judgment-form NBt
  #:mode (types I O)
  #:contract (types t T)
  [----------------- "T-True"
                     (types true Bool)]
  [----------------- "T-False"
                     (types false Bool)]
  [(types t_1 Bool)
   (types t_2 T_1)
   (types t_3 T_1)
   ---------------- "T-If"
   (types (if t_1 then t_2 else t_3) T_1)]
  [------------- "T-Zero"
   (types 0 Nat)]
  [(types t_1 Nat)
   ----------------- "T-Succ"
   (types (succ t_1) Nat)]
  [(types t_1 Nat)
   ----------------- "T-Pred"
   (types (pred t_1) Nat)]
  [(types t_1 Nat)
   ----------------- "T-IsZero"
   (types (iszero t_1) Bool)]
  )

(define-union-language NBtr
  NBr NBt)

(test-equal
 (judgment-holds (types true Bool))
 #t)
(test-equal
 (judgment-holds (types 0 Nat))
 #t)
(test-equal
 (judgment-holds (types true Nat))
 #f)
(test-equal
 (judgment-holds (types (succ true) Nat))
 #f)
(test-equal
 (judgment-holds (types (succ (if true
                                    then (pred 0)
                                    else (succ (pred (succ 0)))))
                          Nat))
 #t)

(test-results)
