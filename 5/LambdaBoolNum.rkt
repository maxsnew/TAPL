#lang racket

(require racket
         "Church.rkt")

(define ω
  `((λ x (x x))
    (λ x (x x))))

(define fix
  `(λ f
      (λ x
         (f (λ y
               ((x x) y))))
      (λ x
         (f (λ y
               ((x x) y))))))

;; Factorial with fix
(define fact
  `(,fix
    (λ fct
       (λ n
          (if ((realeq n) ,zero)
              then ,one
              else ((,times n)
                    (fct (,prd n))))))))
;; Factorial using test instead of if
;; Ex 5.2.9
(define fact2
  `(,fix
    (λ fct
       (λ n
          (((,tst ((,eql n) ,zero))
            (λ x ,zero))
           (λ x
              (((,times n)
                (fct (,prd n)))
               x)))))))

;; Churchnat
;; Natural number to church numeral
(define churchnat
  `(fix
    (λ churchnat
       (λ n
          (if ((= n) 0)
              then ,zero
              else (,succ (churchnat (,prd n))))))))

;; Fixity sum
(define sm
  `(fix
    (λ sum
       (λ l
          (if (,isnil? l)
              then ,zero
              else ((,plus (,hd l))
                    (sum (,tl l))))))))
;; Foldy sum
(define sm2
  `(λ l
      ((l ,plus) ,zero)))
