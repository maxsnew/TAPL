#lang racket

(require redex)

;; The Boolean language in Figure 3-1

(define-language B
  (t true
     false
     (if t then t else t))
  (v true false))

(define R
  (reduction-relation
   B
   (--> (if true then t_1 else t_2)
        t_1
        "E-IfTrue")
   (--> (if false then t_1 else t_2)
        t_2
        "E-IfFalse")))

;; TESTS
;; Term sanity tests
(test-equal
 (redex-match?
  B
  t
  (term (if true then false else false)))
 #t)

(test-equal
 (redex-match?
  B
  v
  (term (if true then false else true)))
 #f)

(test-equal
 (redex-match?
  B
  t
  (term true))
 #t)

(test-results)

