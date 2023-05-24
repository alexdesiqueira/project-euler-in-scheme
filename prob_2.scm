(define (sum-even-fibonacci-numbers-up-to number)
  (define (fibonacci-sum-helper a b sum)
    (if (> b number)
        sum
        (if (even? b)
            (fibonacci-sum-helper b (+ a b) (+ sum b))
            (fibonacci-sum-helper b (+ a b) sum))))

  (fibonacci-sum-helper 1 1 0))

(display (sum-even-fibonacci-numbers-up-to 4000000))
(newline)

