#lang typed/racket

(define-type Expr (U Value If Succ Pred IsZero))
(define-type Value (U Boolean Natural))
(define-predicate Value? Value)

(struct: If ([pred : Expr]
             [thn  : Expr]
             [els  : Expr])
         #:transparent)
(struct: Succ ([e : Expr]) #:transparent)
(struct: Pred ([e : Expr]) #:transparent)
(struct: IsZero ([e : Expr]) #:transparent)

;; Single step

;; (: eval (Expr -> Expr))
;; (define (eval b)
;;   (cond [(Value? b) b]
;;         [else
;;          (match-let ([(If prd thn els) b])
;;            (cond [(Value? prd)
;;                   (if prd
;;                       thn
;;                       els)]
;;                  [else
;;                   (If (eval prd)
;;                       thn
;;                       els)]))]))

;; Closure of singlestep

;; (: eval* (Expr -> Value))
;; (define (eval* b)
;;   (cond [(Value? b) b]
;;         [else (eval* (eval b))]))

;; Bigstep
(: Eval (Expr -> Value))
(define (Eval expr)
  (: expecting-nat ((Natural -> Value) Expr -> Value))
  (define (expecting-nat f expr)
    (let ([val (Eval expr)])
      (if (number? val)
          (f val)
          (error "Succ expects nat, got: " val))))  
  (cond [(Value? expr) expr]
        [(If? expr)
         (match-let ([(If prd thn els) expr])
           (let ([prd-val (Eval prd)])
             (if (boolean? prd-val)
                 (if prd-val
                     (Eval thn)
                     (Eval els))
                 (error "If takes a boolean, got: " prd-val))))]
        [(Succ? expr)
         (let* ([sub (Succ-e expr)]
                [subval (Eval sub)])
           (if (number? subval)
               (+ subval 1)
               (error "Succ expects nat, got: " subval)))]
        [(Pred? expr)
         (expecting-nat (λ (val)
                           (if (= 0 val)
                               0
                               (- val 1)))
                        (Pred-e expr))]
        [(IsZero? expr)
         (expecting-nat zero?
                        (IsZero-e expr))]))

(Eval {If {If #t #t #f}
          #t
          #f})
(Eval {If (IsZero (Pred 0))
          #t
          #f})
(Eval (IsZero 0))
(Eval (IsZero (Pred 0)))
