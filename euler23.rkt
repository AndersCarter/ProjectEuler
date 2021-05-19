#lang racket

(require test-engine/racket-tests)

; int -> [int]
; Returns the proper divsors of
(check-expect (proper-divisors 12) (list 1 2 3 4 6))
(check-expect (proper-divisors 20) (list 1 2 4 5 10))

(define (proper-divisors n [i 1] [divisors empty])
  (cond [(> i (floor (/ n 2))) (reverse divisors)]
        [(= (modulo n i) 0) (proper-divisors n (+ i 1) (cons i divisors))]
        [else (proper-divisors n (+ i 1) divisors)]))

; int -> bool
; Determines if 'n' is abundant
(check-expect (is-abundant? 12) #t)
(check-expect (is-abundant? 28) #f)

(define (is-abundant? n)
  (> (foldr + 0 (proper-divisors n)) n))

; int -> [int]
; Finds all abundant numbers <= 'n'
(define (all-abundant n)
  (reverse (for/fold ([abundant-numbers empty])
                     ([i n])
             (if (is-abundant? (+ i 1))
                 (cons (+ i 1) abundant-numbers)
                 abundant-numbers))))

; int -> [int]
; Finds all values that are the sum of two abundant numbers smaller than 'n'

(define (sum-two-abundant n)
  (define n-abundants (list->vector (all-abundant n)))
  (sum-two-abundant-recur n n-abundants))

(define (sum-two-abundant-recur n abundants [i 0] [j 0] [abundant-pairs (set)])
  (cond [(>= i (vector-length abundants)) abundant-pairs]
        [(>= j (vector-length abundants)) (sum-two-abundant-recur n abundants (+ i 1) 0 abundant-pairs)]
        [else (define i-val (vector-ref abundants i))
              (define j-val (vector-ref abundants j))
              (define ij-sum (+ i-val j-val))
              (if (< ij-sum n)
                  (sum-two-abundant-recur n abundants i (+ j 1) (set-add abundant-pairs ij-sum))
                  (sum-two-abundant-recur n abundants i (+ j 1) abundant-pairs))]))

(test)

(define n 28123)
(define abundant-pairs (sum-two-abundant n))
(for/sum ([i (- n 1)])
  (if (set-member? abundant-pairs i)
      0
      i))

