#lang racket

(require test-engine/racket-tests)

; string -> [string]
; Creates the permutation of each
(check-expect (string-permutations "012") (list "012" "021" "102" "120" "201" "210"))

(define (string-permutations str)
  (define permutes (permutations (string-length str)))
  (sort (for/fold ([acc empty])
                  ([permutation permutes])
          (cons (list->string (map (lambda (x) (string-ref str x)) permutation)) acc))
        string<?))

; int -> [[int]]
; Generates all permutations of a set with 'n' items.
(check-expect (permutations 2) (list (list 0 1) (list 1 0)))
(check-expect (permutations 3) (list (list 0 1 2) (list 1 0 2) (list 0 2 1) (list 2 0 1) (list 1 2 0) (list 2 1 0)))

(define (permutations n [index-set (set)] [permutation empty] [depth 0])
  (cond [(>= depth n) (list permutation)]
        [else (for/fold ([acc empty])
                        ([i n])
                (if (set-member? index-set i)
                    acc
                    (append (permutations n (set-add index-set i) (cons i permutation) (+ depth 1)) acc)))]))

(test)

(list-ref (string-permutations "0123456789") 999999)

