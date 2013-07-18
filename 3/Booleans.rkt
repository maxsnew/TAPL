#lang racket

(require redex)

;; The Boolean language in Figure 3-1

(define-language B
  (t true
     false
     (if t then t else t))
  (v true false))

;; Evaluation context
;; This encodes rule "E-If"
(define-extended-language Br
  B
  (p (if p then t else t)
     hole))

(define reductions
  (reduction-relation
   Br
   (==> (if true then t_1 else t_2)
        t_1
        "E-IfTrue")
   (==> (if false then t_1 else t_2)
        t_2
        "E-IfFalse")
   with
   [(--> (in-hole p_1 a) (in-hole p_1 b))
    (==> a b)]))

;; TESTS
;; Term sanity tests
(test-equal
 (redex-match?
  Br
  t
  (term (if true
            then false
            else false)))
 #t)

(test-equal
 (redex-match?
  Br
  v
  (term (if true
            then false
            else true)))
 #f)

(test-equal
 (redex-match?
  Br
  t
  (term true))
 #t)

;; Reduction relation tests
(test-->>
 reductions
 (term true)
 (term true))

(test-->>
 reductions
 (term false)
 (term false))

(test-->>
 reductions
 (term (if true
           then true
           else false))
 (term true))

(test-->>
 reductions
 (term (if false
           then false
           else true))
 (term true))

(test-->>
 reductions
 (term (if (if true
               then true
               else false)
           then true
           else false))
 (term true))

(test-->>
 reductions
 (term (if true
           then (if true
                    then true
                    else false)
           else false))
 (term true))

(test-results)
