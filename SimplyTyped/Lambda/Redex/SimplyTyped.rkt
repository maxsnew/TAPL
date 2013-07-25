#lang racket

(require redex
         redex/tut-subst)

(provide STλcR
         λcCBVal)

;; Figure 9-1
(define-language pureSTλc
  (t x
     (λ x : T t)
     (t t))
  (v (λ x T t))
  (T (T → T))
  (Γ ∅
     (Γ x : T))
  (x variable-not-otherwise-mentioned))

(define-extended-language STλc
  pureSTλc
  (t ....
     true
     false
     (if t
         then t
         else t))
  (v ....
     true
     false)
  (T ....
     Bool))

(define-extended-language STλcR
  STλc
  (E (E t)
     (if E then t else t)
     ((λ x : T t) E)
     hole))

(define λcCBVal
  (reduction-relation
   STλcR
   #:domain t
   (==> ((λ x : T t) v)
        (subst x v t)
        "β-Reduction")
   (==> (if true then t_1 else t_2)
        t_1
        "E-If-True")
   (==> (if true then t_1 else t_2)
        t_2
        "E-If-False")
   with
   [(--> (in-hole E_1 a) (in-hole E_1 b))
    (==> a b)]))

(define-metafunction STλcR
  subst : x v t -> t
  [(subst x v t)
   ,(subst/proc x? (list (term x)) (list (term v)) (term t))])

(define x? (redex-match pureSTλc x))

(define-judgment-form STλc
  #:mode (types I I O)
  #:contract (types Γ t T)
  [----------------- "T-True"
   (types Γ true Bool)]
  [----------------- "T-False"
   (types Γ false Bool)]
  [(types Γ t_1 Bool)
   (types Γ t_2 T_1)
   (types Γ t_3 T_1)
   ---------------- "T-If"
   (types Γ (if t_1 then t_2 else t_3) T_1)]
  
  [------------------------- "Τ-Var-1"
   (types (Γ x : T) x T)]
  [(types Γ x_1 T_1)
   (side-condition (different x_1 x_2))
   ------------------------------------ "T-Var-2"
   (types (Γ x_2 : T_2) x_1 T_1)]

  [(types (Γ x : T_1) t T_2)
   ----------------------------- "T-Abs"
   (types Γ (λ x : T_1 t) (T_1 → T_2))]

  [(types Γ t_1 (T_1 → T_2))
   (types Γ t_2 T_1)
   ------------------------- "Τ-App"
   (types Γ (t_1 t_2) T_2)])

;; Exercises 9.2.1-2
(test-equal
 (judgment-holds (types ∅ ((λ x : Bool x) true) Bool))
 #t)
(test-equal
 (judgment-holds (types (∅ f : (Bool → Bool))
                        (f (if false
                               then true
                               else false))
                        Bool))
 #t)
(test-equal
 (judgment-holds (types (∅ f : (Bool → Bool))
                        (λ x : Bool
                           (f (if x
                                  then false
                                  else x)))
                        (Bool → Bool)))
 #t)

(test-results)

