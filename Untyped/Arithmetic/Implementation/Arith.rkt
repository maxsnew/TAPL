n#lang racket

(struct If (prd thn els) #:transparent)
(struct Succ (e) #:transparent)
(struct Pred (e) #:transparent)
(struct IsZero (e) #:transparent)

(struct Zero  () #:transparent)
(struct True () #:transparent)
(struct False () #:transparent)

;; Untyped racket :(
(define (val? e)
  (or (Boolval? e)
      (Natval? e)))

(define (Boolval? e)
  (or (True? e)
      (False? e)))

(define (Natval? e)
  (or (Zero? e)
      (and (Succ? e)
           (Natval? (Succ-e e)))))

;; Big step
(define (Eval e)
  (if (val? e)
      e
      (match e
        [(If prd thn els)
         (if (Eval prd)
             (Eval thn)
             (Eval els))]
        [(Succ e)
         (Succ (Eval e))]
        [(Pred e-n)
         (let ([n (Eval e-n)])
           (match n
             [(Succ m) m]
             [(Zero) n]))]
        [(IsZero e)
         (wrapBool (let ([e (Eval e)])
                     (Zero? e)))])))

(define (wrapBool b)
  (if b (True) (False)))

(define (wrapNat n)
  (if (= 0 n)
      (Zero)
      (Succ (wrapNat (- n 1)))))

(define (natural? n)
  (and (integer? n)
       (>= n 0)))

(define (parse t)
  (cond [(boolean? t) (wrapBool t)]
        [(natural? t) (wrapNat t)]
        [else
         (match t
           [`(if ,prd ,thn ,els)
            (If (parse prd)
                (parse thn)
                (parse els))]
           [`(succ ,n)
            (Succ (parse n))]
           [`(pred ,n)
            (Pred (parse n))]
           [`(iszero ,n)
            (IsZero (parse n))])]))

(define (eval sexp)
  (Eval (parse sexp)))
