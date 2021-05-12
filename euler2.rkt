#lang racket

(require test-engine/racket-tests)

; num -> num
; Sums all even fibonacci numbers that do not exceed n
(check-expect (even-fib-sum 100) 44)

(define (even-fib-sum n [acc 2] [prev-fib 1] [current-fib 2])
  (define next-fib (+ prev-fib current-fib))
  (cond [(>= next-fib n) acc]
        [(even-fib-sum n
                       (if (even? next-fib) (+ acc next-fib) acc)
                       current-fib
                       next-fib)]))

(test)
(even-fib-sum 4000000)