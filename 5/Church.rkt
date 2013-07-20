#lang racket
(require redex
         redex/tut-subst)

(provide (all-defined-out))

;; Page 53
(define-language λc
  (t x
     (λ x t)
     (t t))
  (v (λ x t))
  (x variable))

;; Call by value
(define-extended-language λcR
  λc
  (E (E t)
     ((λ x t) E)
     hole))

(define λcCBVal
  (reduction-relation
   λcR
   #:domain t
   (==> ((λ x t) v)
        (subst x v t)
        "β-Reduction")
   with
   [(--> (in-hole E_1 a) (in-hole E_1 b))
    (==> a b)]))

;; Skipping this for now, I'll do it in the interpreter!
(define-metafunction λcR
  subst : x v t -> t
  [(subst x v t)
   ,(subst/proc x? (list (term x)) (list (term v)) (term t))])

(define x? (redex-match λc x))

;; Fun with combinators!
(define tru
  `(λ t (λ f t)))

(define fls
  `(λ t (λ f f)))

(define tst
  `(λ l
      (λ m
         (λ n
            ((l m) n)))))

(define nd
  `(λ b
      (λ c
         ((b c) ,fls))))

;; Exercise 5.2.1
;; or
(define o
  `(λ b
      (λ c
         ((b ,tru) c))))

;; no
(define no
  `(λ b
      ((b ,fls) ,tru)))

;; Pairs
(define pair
  `(λ f
      (λ s
         (λ b
            ((b f) s)))))

(define fst
  `(λ p
      (p ,tru)))

(define snd
  `(λ p
      (p ,fls)))

;; Church numerals
(define zero
  `(λ s
      (λ z z)))

(define one
  `(λ s
      (λ z (s z))))

(define two
  `(λ s
      (λ z (s (s z)))))

(define succ
  `(λ n
      (λ s
         (λ z
            (s ((n s) z))))))

(define succ2
  `(λ n
      (λ s
         (λ z
            ((n s) (s z))))))

(define plus
  `(λ m
      (λ n
         (λ s
            (λ z
               ((m s) ((n s) z)))))))

(define times
  `(λ m
      (λ n
         ((m (,plus n)) ,zero))))

;; multiplication is function composition!
(define times2
  `(λ m
      (λ n
         (λ s
            (m (n s))))))

;; Ex 5.2.4
;; λ m n . m^n
(define xp
  `(λ m
      (λ n
         ((n (times m)) ,one))))

;; exponentation is function application!
(define xp2
  `(λ m
      (λ n
         (m n))))

(define iszro
  `(λ m
      ((m (λ s ,fls)) ,tru)))

(define prd
  (let ([zz `((,pair ,zero) ,zero)]
        [ss `(λ p
                ((,pair (,snd p))
                 ((,plus ,one)
                  (,snd p))))])
    `(λ m
        (,fst ((m ,ss) ,zz)))))

;; Ex 5.2.5
(define sub
  `(λ m
      (λ n
         ((n ,prd) m))))

;; Ex 5.2.7
(define eql
  `(λ m
      (λ n
         ((,nd (,iszro ((,sub m n))))
          (,iszro ((,sub n) m))))))

;; Lists !
;; Ex 5.2.8
;; cons
(define cns
  `(λ x
      (λ l
         (λ c
            (λ n
               ((c x)
                ((l c) n)))))))
;; nil, empty list
;; nil = false = 0!!!
(define nl
  `(λ c
      (λ n n)))

(define isnil?
  `(λ l
      ((l (λ x ,fls))
       ,tru)))

;; head, first element of a list
;; returns first element of a nonempty list
;; returns false otherwise
(define hd
  `(λ l
      ((l ,tru) ,fls)))

;; tail, analagous to predecessor
(define tl
  `(λ l
      (,fst
       (l (λ x
             (λ p
                ((,pair (,snd p))
                 ((,cns x) (,snd p)))))
          ((,pair ,nl) ,nl)))))

;; Note some tests fail, but they are all α-equivalent
;; Should maybe write an α-equivalence checker

;; true, false, test
(test-->>
 λcCBVal
 `(((,tst ,tru) v) w)
 (term v))

(test-->>
 λcCBVal
 `(((,tst ,fls) v) w)
 (term w))

;; and, or, not
(test-->>
 λcCBVal
 `((,nd ,tru) ,tru)
 tru)

(test-->>
 λcCBVal
 `((,nd ,tru) ,fls)
 fls)

(test-->>
 λcCBVal
 `((,o ,tru) ,fls)
 tru)

(test-->>
 λcCBVal
 `((,o ,fls) ,fls)
 fls)

(test-->>
 λcCBVal
 `(,no ,fls)
 tru)

(test-->>
 λcCBVal
 `(,no ,tru)
 fls)

;; pair, first, second
(test-->>
 λcCBVal
 `(,fst ((,pair v) w))
 `v)

(test-->>
 λcCBVal
 `(,snd ((,pair v) w))
 `w)

;; numerals and successor are more difficult to test mechanically.

(test-results)
