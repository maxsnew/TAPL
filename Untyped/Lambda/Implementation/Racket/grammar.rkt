#lang typed/racket

(provide (all-defined-out))

(define-type Expr (U Var FreeVar Abs App))
(define-type Value Abs)
(define-type FreeVar String)
(struct: Var ([num : Natural]
              [name : String]))
(struct: Abs ([bod : Expr]))
(struct: App ([rator : Expr]
              [rand  : Expr]))
