(define (divide-without-remainder number divisor)
  (if (equal? (modulo number divisor) 0)
      #t
      #f))

(define (divide-without-remainder-interval number begin end)
  (display (list number begin end))
  (newline)
  (cond
   ((>= begin end) #t)
   ((equal? (divide-without-remainder number begin) #t)
    (divide-without-remainder-interval number (+ 1 begin) end))
   (else #f)))

(define (smallest-positive-evenly-divisible? begin end)
  (define (smallest-positive-helper guess)
    (if (equal? (divide-without-remainder-interval guess begin end) #t)
        guess
        (smallest-positive-helper (+ 2520 guess))))
  (smallest-positive-helper 2520))   ;; I mean, it should be higher than 2520 :)

;; tests
(display (divide-without-remainder 4 2))  ;; should be #t
(newline)
(display (divide-without-remainder 6 4))  ;; should be #f
(newline)
(display (divide-without-remainder-interval 2520 1 10))  ;; should be #t
(newline)
(display (divide-without-remainder-interval 250 1 10))  ;; should be #f
(newline)
(display (smallest-positive-evenly-divisible? 1 10))  ;; should be 2520
(newline)

;; result
(display (smallest-positive-evenly-divisible? 11 20))
