#lang racket

(require test-engine/racket-tests)

; int -> [int]
; Generates all primes <= to by 'n'.
(check-expect (eratosthenes 20) (list 2 3 5 7 11 13 17 19))

(define (eratosthenes n)
  (define primality-flags (primality-findprimes n))
  (reverse (for/fold ([acc empty])
                     ([i (+ n 1)])
             (if (vector-ref primality-flags i)
                 (cons i acc)
                 acc))))

; Updates the primality flags for the new prime
(define (primality-update! flags prime)
  (primality-update!-recur flags (+ prime prime) prime))

(define (primality-update!-recur flags i step)
  (cond [(>= i (vector-length flags)) (void)]
        [else (vector-set! flags i #f)
              (primality-update!-recur flags (+ i step) step)]))

; Determines the next prime from the last one
(define (primality-nextprime flags offset)
  (cond [(>= offset (vector-length flags)) (vector-length flags)]
        [(vector-ref flags offset) offset]
        [else (primality-nextprime flags (+ offset 1))]))

; finds all primes <= n
(define (primality-findprimes n)
  (define flags (make-vector (+ n 1) #t))
  (vector-set! flags 0 #f)
  (vector-set! flags 1 #f)
  (primality-findprimes-recur flags 2)
  )

(define (primality-findprimes-recur flags next-prime)
  (cond [(>= next-prime (vector-length flags)) flags]
        [else (primality-update! flags next-prime)
              (primality-findprimes-recur flags (primality-nextprime flags (+ next-prime 1)))]))

; Rotates the integer by one digit
(check-expect (integer-rotations 2) (list 2))
(check-expect (integer-rotations 197) (list 197 719 971))
(check-expect (integer-rotations 1234) (list 1234 4123 3412 2341))

(define (integer-rotations n)
  (integer-rotations-recur n (list n) (- (digit-count n) 1)))

(define (integer-rotations-recur n rotations digit-count [i 0])
  (cond [(>= i digit-count) (reverse rotations)]
        [else (define last-digit (modulo n 10))
              (define next-rotation (+ (* last-digit (expt 10 digit-count))
                                       (floor (/ n 10))))
              (integer-rotations-recur next-rotation (cons next-rotation rotations) digit-count  (+ i 1))]))

; Counts the number of digits
(define (digit-count n)
  (inexact->exact (+ (floor (log n 10)) 1)))

; Determines if the integer is prime
(check-expect (is-prime? 10) #f)
(check-expect (is-prime? 17) #t)

(define (is-prime? n [i 2])
  (cond [(>= i n) #t]
        [(= (modulo n i) 0) #f]
        [else (is-prime? n (+ i 1))]))

; Generates a list of all circular primes < 'n'
(check-expect (circular-primes 100) (list 2 3 5 7 11 13 17 31 37 71 73 79 97))

(define (circular-primes n)
  (define primes (eratosthenes (- n 1)))
  (define prime-set (list->set primes))
  (reverse (for/fold ([acc empty])
                     ([prime primes])
             (define rotations (integer-rotations prime))
             (if (foldr (lambda (x y) (and (set-member? prime-set x) y)) #t (rest rotations))
                 (cons prime acc)
                 acc))))

(test)
(length (circular-primes 1000000))