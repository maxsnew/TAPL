#lang racket

(require redex
         "Booleans.rkt")

;; From Figure
(define-extended-language NB
  Br ;; I guess keep the p here, maybe use union language
  (t .... ;; keep all the old booleans
     0
     (succ t)
     (pred t)
     (iszero t))
  (v ....
     nv)
  (nv 0
      (succ nv)))

;; Encodes E-Succ, E-Pred, E-IsZero
(define-extended-language NBr
  NB
  (p ....
     (succ p)
     (pred p)
     (iszero p)))

(define NBreductions
  (extend-reduction-relation
   Breductions
   NBr
   (==> (pred 0)
        0
        "E-PredZero")
   (==> (pred (succ nv_1))
        nv_1
        "E-PredSucc")
   (==> (iszero 0)
        true
        "E-IsZeroZero")
   (==> (iszero (succ nv_1))
        false)
   with
   [(--> (in-hole p_1 a) (in-hole p_1 b))
    (==> a b)]))

;; Tests
(test-equal
 (redex-match?
  NB
  t
  (term (pred 0)))
 #t)

(test-equal
 (redex-match?
  NB
  nv
  (term (succ 0)))
 #t)

(test-equal
 (redex-match?
  NB
  nv
  (term (iszero 0)))
 #f)

(test-->>
 NBreductions
 (term (pred (succ (pred 0))))
 0)

(test-->>
 NBreductions
 (term (iszero (succ (pred 0))))
 (term false))

(test-results)
