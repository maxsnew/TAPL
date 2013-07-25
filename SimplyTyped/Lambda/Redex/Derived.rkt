#lang racket

(require redex
         redex/tut-subst
         "SimplyTyped.rkt")

(define-extended-language STλcR+
  STλcR
  (t ....
     unit
     (t as T)
     (cons t t)
     (fst t)
     (snd t)
     (left t)
     (right t)
     (case t of (left x)  => t
           ! (right x) => t))
  (v ....
     unit
     (cons v v)
     (left v)
     (right v))
  (T ....
     unit
     (T × T)
     (T + T))
  (E ....
     (E as T)
     (cons E t)
     (cons v E)
     (fst E)
     (snd E)
     (left E)
     (right E)
     (case E of (left x) => t
           ! (right x) => t)))

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
   (==> (case (left v) of (left x_1) => t_1
              ! (right x_2) => t_2)
        (subst x_1 v t_1)
        "E-Right")
   (==> (case (right v) of (left x_1) => t_1
              ! (right x_2) => t_2)
        (subst x_2 v t_2)
        "E-Left")
   with
   [(--> (in-hole E_1 a) (in-hole E_1 b))
    (==> a b)]))

(define-metafunction STλcR+
  subst : x v t -> t
  [(subst x v t)
   ,(subst/proc x? (list (term x)) (list (term v)) (term t))])

(define x? (redex-match STλcR+ x))

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
   (types Γ (snd t) T_2)]
  [(types Γ t (T_1 + T_2))
   ----------------------- "T-Left"
   (types Γ (left t) T_1)]
  [(types Γ t (T_1 + T_2))
   ----------------------- "T-Right"
   (types Γ (right t) T_1)]
  [(types Γ t_1 (T_1 + T_2))
   (types (Γ x_1 : T_1) t_2 T_3)
   (types (Γ x_2 : T_1) t_3 T_3)
   ----------------------- "T-Case"
   (types Γ
          (case t_1 of (left x_1)  => t_2
                ! (right x_2) => t_3)
          T_3)])

;; Exercise 11.9.1
(define Bool
  `(unit + unit))
(define tru
  `(left unit))
(define fls
  `(right unit))
(define iff
  `(λ x : ,Bool
      (case x of (left _) (λ _ : unit
                             then)
            ! (right _) (λ _ : unit
                           else))))
