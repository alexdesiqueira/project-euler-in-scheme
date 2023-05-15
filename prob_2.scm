(define (sum-even-fibonacci-numbers-up-to number)
  (sum-iter 0 1 number))


;; broken. need to check
(define (sum-iter sum curr_fibo number)
  (display sum)
  (newline)
  (display curr_fibo)
  (newline)
  (display number)
  (newline)
  (newline)
  (if (and (<= (fibonacci curr_fibo) number) (even? curr_fibo))
      (sum-iter (+ sum curr_fibo) (fibonacci (+ curr_fibo 1)) number)
      (sum-iter sum (fibonacci (+ curr_fibo 1)) number)))


;; this fibonacci definition is given in SICP, page 39
;; adapted: fib_1 = 1; fib_2 = 1
(define (fibonacci number)
  (fib-iter 1 1 number))

(define (fib-iter a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))


;; tests
;;(display (fibonacci 10))
(display (sum-even-fibonacci-numbers-up-to 11))
