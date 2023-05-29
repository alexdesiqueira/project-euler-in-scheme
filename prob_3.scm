;; The prime factors of 13195 are 5, 7, 13 and 29.
;; What is the largest prime factor of the number 600851475143 ?

; remembering the _fundamental theorem of arithmetic_:
; "every integer greater than 1 can be represented uniquely as a
; product of prime numbers, up to the order of the factors.
; For example, 1200 = 2 ^ 4 * 3 ^ 1 * 5 ^ 2"
;
; see <https://en.wikipedia.org/wiki/Fundamental_theorem_of_arithmetic>

; a naive solution. first, we check if the number is prime; if yes, we
; check if it divides the input.

; defining a function to check if the number is prime.
; adapted from SICP, 1.2.6:
(define (prime? number)

  (define (smallest-divisor number)
    (define (divides? a b)
      (= (remainder b a) 0))
    (define (find-divisor number test-divisor)
      (cond ((> (square test-divisor) number) number)
            ((divides? test-divisor number) test-divisor)
            (else (find-divisor number (+ test-divisor 1)))))
    (define (square number)
      (* number number))
    (find-divisor number 2))

  (if (and (even? number) (not (equal? number 2)))
      #f
      (= number (smallest-divisor number))))


(display (prime? 65533))

;; checking lists in Guile:
;; https://www.gnu.org/software/guile/manual/html_node/List-Syntax.html
;; Do we need a list to add the elements? Maybe an accumulator is sufficient
;; (see _Fundamental theorem of arithmetic_)

(define (prime-factors number)
  (define (factors-helper count number)
    (cond
     ((> count number) count)
     ((and (prime? count) (not (equal? (modulo number count))))
      (begin
        (display number)
        (factors-helper (+ count 1) number)))
      (else (factors-helper (+ count 1) number))))
  (factors-helper 1 number))

(display (prime-factors 65533))
