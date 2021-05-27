#lang racket

(require test-engine/racket-tests)

; Constants
(define TEST-TRIANGLE (vector (vector 3)
                              (vector 7 4)
                              (vector 2 4 6)
                              (vector 8 5 9 3)))

; vector -> int
; Determines the maximum path sum of the triangle (represented by two dimensional vector).
(check-expect (maximum-path-sum TEST-TRIANGLE) 23)

(define (maximum-path-sum triangle [i 0] [depth 0] [sum 0])
  (cond [(>= depth (vector-length triangle)) sum]
        [else (define val (vector-ref (vector-ref triangle depth) i))
              (max (maximum-path-sum triangle i (+ depth 1) (+ sum val))
                   (maximum-path-sum triangle (+ i 1) (+ depth 1) (+ sum val)))]))

; string -> vector
; Parses the triangle into a two dimensional vector
(define (parse-triangle str)
  (list->vector
   (reverse
    (for/fold ([acc empty])
              ([line (string-split str "\n")])
      (cons (list->vector (map string->number (string-split line " "))) acc)))))

; Execution
(define triangle-str
  "75
95 64
17 47 82
18 35 87 10
20 04 82 47 65
19 01 23 75 03 34
88 02 77 73 07 63 67
99 65 04 28 06 16 70 92
41 41 26 56 83 40 80 70 33
41 48 72 33 47 32 37 16 94 29
53 71 44 65 25 43 91 52 97 51 14
70 11 33 28 77 73 17 78 39 68 17 57
91 71 52 38 17 14 91 43 58 50 27 29 48
63 66 04 68 89 53 67 30 73 16 69 87 40 31
04 62 98 27 23 09 70 98 73 93 38 53 60 04 23")

(maximum-path-sum (parse-triangle triangle-str))

(test)