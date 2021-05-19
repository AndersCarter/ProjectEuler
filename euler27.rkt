#lang racket

(require test-engine/racket-tests)

; int int -> int
; Determines the number of consecutive primes from the formula 'n**2 + a * n + b'.
(check-expect (consecutive-quadradic-primes 1 41) 40)
(check-expect (consecutive-quadradic-primes -79 1601) 80)

(define (consecutive-quadradic-primes a b [i 0])
  (define result (+ (* i i) (* i a) b))
  (if (is-prime? result)
      (consecutive-quadradic-primes a b (+ i 1))
      i))

; int -> bool
; Determines if the given value is a prime
(check-expect (is-prime? 12) #f)
(check-expect (is-prime? 13) #t)
(check-expect (is-prime? -13) #f)

(define (is-prime? n [i 2])
  (cond [(<= n 1) #f]
        [(>= i n) #t]
        [(= (modulo n i) 0) #f]
        [else (is-prime? n (+ i 1))]))

; void -> int
; Finds the product of the two quadratic coefficients that produce the most consecutive primes between where |a| < 1000 and |b| <= 1000

(define (find-coefficients [a -999] [b -1000] [max-count -1] [max-product -1])
  (cond [(>= a 1000) max-product]
        [(> b 1000) (find-coefficients (+ a 1) -1000 max-count max-product)]
        [else (define prime-count (consecutive-quadradic-primes a b))
              (if (> prime-count max-count)
                  (find-coefficients a (+ b 1) prime-count (* a b))
                  (find-coefficients a (+ b 1) max-count max-product))]))

(test)

(find-coefficients)