#lang racket

(require redex
         "../3/Booleans.rkt")

(provide Bt)

;; Figure 8-2
(define-extended-language Bt
  B
  (T Bool))

(define-judgment-form Bt
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
  )
