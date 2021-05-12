#lang racket

(require test-engine/racket-tests)

; num -> num
; Sums all the natural numbers below 'n' that are multiples of 3 or five
(check-expect (sum-multiples-3-or-5 10) 23)

(define (sum-multiples-3-or-5 n)
  (for/sum ([i n])
    (if (or (= (modulo i 3) 0) (= (modulo i 5) 0)) i 0)))

; Execution
(test)
(sum-multiples-3-or-5 1000)