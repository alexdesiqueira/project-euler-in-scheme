(define (sum-multiples-of-3-or-5-below number)
  (sum-iter 0 (- number 1)))  ;; returning all the multiples _below_ number

(define (sum-iter sum count)
  (cond ((= count 0) sum)
        ((multiple-of-3-or-5? count)
         (sum-iter (+ sum count) (- count 1)))
        (else (sum-iter sum (- count 1)))))

(define (multiple-of-3-or-5? number)
  (if (or (multiple-of-3? number) (multiple-of-5? number))
      #t
      #f))

(define (multiple-of-3? number)
  (if (= (modulo number 3) 0)
      #t
      #f))

(define (multiple-of-5? number)
  (if (= (modulo number 5) 0)
      #t
      #f))

(display (sum-multiples-of-3-or-5-below 1000))
(newline)


;; tests: these are supposed to be #t
(display (multiple-of-3? 15))
(newline)

(display (multiple-of-5? 25))
(newline)

(display (multiple-of-3-or-5? 9))
(newline)

(display (sum-multiples-of-3-or-5-below 10))  ;; this is 23
(newline)

;; tests: these are supposed to be #f
(display (multiple-of-3? 11))
(newline)

(display (multiple-of-5? 14))
(newline)

(display (multiple-of-3-or-5? 13))
(newline)
