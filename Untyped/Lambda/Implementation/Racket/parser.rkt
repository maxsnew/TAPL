#lang racket

(require srfi/2)

(require "grammar.rkt")

(struct named-abs (var bod))
(struct named-app (rator rand))

;; Sexpr -> NamedExpr
(define (parse-named sexpr)
  (if (symbol? sexpr)
      (symbol->string sexpr)
      (match sexpr
        [`(λ (,v) ,e)
         (if (symbol? v)
             (named-abs (symbol->string v)
                        (parse-named e))
             (error "bad variable in lambda abstraction" sexpr))]
        [`(,e1 ,e2)
         (named-app (parse-named e1)
                    (parse-named e2))])))

;; NamedExpr -> Expr
(define (debrujnify nexpr)
  ;; env : Listof String
  (define (with-env nexpr env)
    (cond [(string? nexpr)
           (or (and-let* ([num (find nexpr env)])
                         (Var num nexpr))
               nexpr)]
          [(named-expr? nexpr)
           (let ([rec-with (λ (f)
                              (with-env (f nexpr) env))])
             (Expr (rec-with nexpr-rator)
                   (rec-with nexpr-rand)))]
          [(named-abs? nexpr)
           (Abs (with-env (named-abs-bod nexpr)
                          (cons (named-abs-var nexpr) env)))]))
  (with-env nexpr '()))

(define (find x lst)
  (define (find-acc lst acc)
    (match lst
      ['() #f]
      [(cons y ys)
       (if (equal? x y)
           acc
           (find-acc ys (+ acc 1)))]))
  (find-acc lst 0))
