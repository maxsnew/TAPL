#lang racket

(require redex
         "SimplyTyped.rkt")

(define-extended-language STλcR+
  STλcR
  (t ....
     unit
     (t as T)
     (cons t t)
     (fst t)
     (snd t))
  (v ....
     unit
     (cons v v))
  (T ....
     unit
     (T × T))
  (E ....
     (E as T)
     (cons E t)
     (cons v E)
     (fst E)
     (snd E)))

(define STλcR+Val
  (extend-reduction-relation
   λcCBVal
   STλcR+
   #:domain t
   (==> (v as T) v
        "E-Ascribe")
   (==> (fst (cons v_1 v_2))
        v_1
        "E-Pair1")
   (==> (snd (cons v_1 v_2))
        v_2
        "E-Pair2")
   with
   [(--> (in-hole E_1 a) (in-hole E_1 b))
    (==> a b)]))

(define-judgment-form STλcR+
  #:mode (types I I O)
  #:contract (types Γ t T)
  [----------------- "T-True"
   (types Γ true Bool)]
  [----------------- "T-False"
                     (types Γ false Bool)]
  [----------------- "T-Unit"
   (types Γ unit unit)]
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
   (types Γ (t_1 t_2) T_2)]
  [(types Γ t T)
   --------------------- "T-Ascribe"
   (types Γ (t as T) T)]
  [(types Γ t_1 T_1)
   (types Γ t_2 T_2)
   ------------------ "T-Pair"
   (types Γ (cons t_1 t_2) (T_1 × T_2))]
  [(types Γ t (T_1 × T_2))
   ----------------------- "T-Proj1"
   (types Γ (fst t) T_1)]
  [(types Γ t (T_1 × T_2))
   ----------------------- "T-Proj2"
   (types Γ (snd t) T_2)])
