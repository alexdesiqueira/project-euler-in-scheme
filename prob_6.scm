(define (square x)
  (* x x))

(define (sum-of-squares begin end)
  (define (sum-helper accum begin)
    (cond
     ((> begin end) accum)
     (else (sum-helper (+ accum (square begin)) (+ begin 1)))))
    (sum-helper 0 begin))

(define (square-of-sum begin end)
  (define (square-helper accum begin)
    (cond
     ((> begin end) (square accum))
     (else (square-helper (+ accum begin) (+ begin 1)))))
  (square-helper 0 begin))

(define (diff-sum-squares-square-sum begin end)
  (- (square-of-sum begin end) (sum-of-squares begin end)))

;; tests
(display (sum-of-squares 1 10))
(newline)
(display (square-of-sum 1 10))
(newline)
(display (diff-sum-squares-square-sum 1 10))
(newline)

;; result
(display (diff-sum-squares-square-sum 1 100))
