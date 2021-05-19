#lang racket

(require test-engine/racket-tests)

; int int -> set
; Determines the ditinct powers of a ^ b between [2,'a-max'] and [2, 'b-max']
(check-expect (distinct-power 5 5) (set 4 8 9 16 25 27 32 64 81 125 243 256 625 1024 3125))

(define (distinct-power a-max b-max [a 2] [b 2] [acc (set)])
  (cond [(> a a-max) acc]
        [(> b b-max) (distinct-power a-max b-max (+ a 1) 2 acc)]
        [else (distinct-power a-max b-max a (+ b 1) (set-add acc (expt a b)))]))

(test)

(set-count (distinct-power 100 100))