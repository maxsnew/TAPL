#lang racket

(require redex)
(provide B
         Br
         Breductions)

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

(define Breductions
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
 Breductions
 (term true)
 (term true))

(test-->>
 Breductions
 (term false)
 (term false))

(test-->>
 Breductions
 (term (if true
           then true
           else false))
 (term true))

(test-->>
 Breductions
 (term (if false
           then false
           else true))
 (term true))

(test-->>
 Breductions
 (term (if (if true
               then true
               else false)
           then true
           else false))
 (term true))

(test-->>
 Breductions
 (term (if true
           then (if true
                    then true
                    else false)
           else false))
 (term true))

(define (all bs)
  (foldl (λ (b1 b2) (and b1 b2))
         #t
         bs))

;; All terms reduce to values
;; And there is only one possible reduction order
(redex-check
 B
 t
 (or (redex-match? B v (term t))
     (let ([reds (apply-reduction-relation* Breductions
                                            (term t))])
       (and (= (length reds) 1)
            (redex-match? B v
                          (car reds))))))

(test-results)
