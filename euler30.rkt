#lang racket

(require test-engine/racket-tests)

; int -> [int]
; Returns a list of all integers that can be written as a sum of the 'n'th power
; of their digits
(check-expect (nth-expt-digits 4) (list 1634 8208 9474))

(define (nth-expt-digits n)
  (define upper-bound (* 4 (expt 9 n)))
  (nth-expt-digits-recur n upper-bound))

(define (nth-expt-digits-recur n upper-bound [i 2] [acc empty])
  (cond [(> i upper-bound) (reverse acc)]
        [else (define digits (int->digits i))
              (define digit-expt-sum (foldr + 0 (map (lambda (x) (expt x n)) digits)))
              (if (= digit-expt-sum i)
                  (nth-expt-digits-recur n upper-bound (+ i 1) (cons i acc))
                  (nth-expt-digits-recur n upper-bound (+ i 1) acc))]))

; int -> [int]
; Returns a list of the digits of 'n'
(check-expect (int->digits 12345) (list 1 2 3 4 5))
(check-expect (int->digits 11111) (list 1 1 1 1 1))

(define (int->digits n [acc empty])
  (cond [(< n 10) (cons n acc)]
        [else (int->digits (floor (/ n 10)) (cons (modulo n 10) acc))]))

(test)

(foldr + 0 (nth-expt-digits 5))