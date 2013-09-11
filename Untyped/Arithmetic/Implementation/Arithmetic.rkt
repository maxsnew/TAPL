#lang typed/racket

(define-type BoolExpr (U Bval Bif))

(struct: Bval ([val : Boolean]))
(struct: Bif ([pred : BoolExpr]
              [thn  : BoolExpr]
              [els  : BoolExpr])
         #:transparent)

;; Single step
(: eval (BoolExpr -> BoolExpr))
(define (eval b)
  (cond [(Bval? b) b]
        [else
         (match-let ([(Bif prd thn els) b])
           (cond [(Bval? prd)
                  (if (Bval-val prd)
                      thn
                      els)]
                 [else
                  (Bif (eval prd)
                       thn
                       els)]))]))

;; Closure of singlestep
(: eval* (BoolExpr -> Boolean))
(define (eval* b)
  (cond [(Bval? b) (Bval-val b)]
        [else (eval* (eval b))]))

;; Bigstep
(: Eval (BoolExpr -> Boolean))
(define (Eval b)
  (cond [(Bval? b) (Bval-val b)]
        [else
         (match-let ([(Bif prd thn els) b])
           (if (Eval prd)
               (Eval thn)
               (Eval els)))]))

(eval* (Bif (Bif (Bval #t)
                 (Bval #t)
                 (Bval #f))
            (Bval #t)
            (Bval #f)))

(Eval (Bif (Bif (Bval #t)
                (Bval #t)
                (Bval #f))
           (Bval #t)
           (Bval #f)))

