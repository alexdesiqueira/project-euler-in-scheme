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

(define (prime-by-index index)
  (define (prime-helper count number)
    (cond
     ((> count index) '())
     ((and (<= count index) (prime? number))
      (begin
        (display (list count number))
        (newline)
        (prime-helper (+ count 1) (+ number 1))))
     (else (prime-helper count (+ number 1)))))
  (prime-helper 1 2))


(display (prime-by-index 10001))
