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

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? number)
  (define (smallest-divisor number)
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

;; checking lists in Guile:
;; https://www.gnu.org/software/guile/manual/html_node/List-Syntax.html
;; Do we need a list to add the elements? Maybe an accumulator is sufficient
;; (see _Fundamental theorem of arithmetic_)

(define (prime-factors number)
  (define (prime-factors-helper count)
    (cond
     ((> count number) '())
     ((and (prime? count) (divides? count number))
      (begin
        (display count)
        (newline)
        (prime-factors-helper (+ count 1))))
      (else (prime-factors-helper (+ count 1)))))
  (prime-factors-helper 2))  ; 1 is not prime!

;; tests
(display (prime? 65533))  ; should be #f
(newline)
(display (divides? 2 6))  ; should be #t
(newline)

(display (prime-factors 13195)) ; should show 5, 7, 13, 29

;; results
(display (prime-factors 600851475143))
