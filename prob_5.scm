(define (divide-without-remainder number divisor)
  (if (equal? (modulo number divisor) 0)
      #t
      #f))

(define (divide-without-remainder-interval number begin end)
  (define (interval-helper count end)
    (display (list number count begin end))
    (newline)
    (if (>= count end)
        #t
        (if (divide-without-remainder number count)
            (divide-without-remainder-interval number (+ 1 count) end)
            #f)))
  (interval-helper begin end))

;; tests
(display (divide-without-remainder 4 2))  ;; should be #t
(newline)
(display (divide-without-remainder 6 4))  ;; should be #f
(newline)
(display (divide-without-remainder-interval 2520 1 10))  ;; should be #t
(newline)
(display (divide-without-remainder-interval 250 1 10))  ;; should be #f
