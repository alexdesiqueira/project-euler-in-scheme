;; first, let's try to figure out a way to check if a number is a palindrome.
;; let's:
;; 1. convert it into a string
;;; to convert a number to a string in scheme, we can use (number->string num).
;; 2. find the middle element
;; 3. separate in two parts according to the middle element.
;;; to separate the string, we can use (substring).
;; then, either:
;; 4.1. reverse a part and compare both strings;
;;; to reverse a string, we can use Scheme's (string-reverse).
;; or:
;; 4.2. compare the i_th element with the len-i_th one.


(define (middle-of-the-string str-input)
  (define str-len (- (string-length str-input) 1))
  (if (integer? (/ str-len 2))
      (/ str-len 2)
      (/ (- str-len 1) 2)))

;; tests
(display (middle-of-the-string "apples"))  ;; should be 2
(newline)
(display (middle-of-the-string "hello"))  ;; should be 2
(newline)
(display (middle-of-the-string "hello world"))  ;; should be 5
(newline)


(define (str-left-half str-input)
  (define str-len (string-length str-input))
  (if (even? str-len)
      (substring str-input
                 0
                 (+ 1 (middle-of-the-string str-input)))
      (substring str-input
                 0
                 (middle-of-the-string str-input))))  ;; (substring string start end)

;; tests
(display (str-left-half "793454397"))  ;; should be 7934
(newline)


(define (str-right-half str-input)
  (define str-len (string-length str-input))
  (substring str-input
             (+ 1 (middle-of-the-string str-input))
             str-len))  ;; 3.

;; tests
(display (str-right-half "hello world"))  ;; should be "world"
(newline)
(display (str-right-half "793454397"))  ;; should be "4397"
(newline)


(define (palindrome? num)
  (define string-input (number->string num))  ;; 1.
  (equal?
   (str-left-half string-input)
   (string-reverse (str-right-half string-input))))  ;; 4.1

;; tests
(display (palindrome? 34547))  ;; should be #f
(newline)
(display (palindrome? 3447))  ;; should be #t
(newline)
(display (palindrome? 3443))  ;; should be #t
(newline)
(display (palindrome? 793454397))  ;; should be #t

;; now, to find the largest palindrome made from the product of
;; 3-digit numbers, we start from the top. Very likely, the
;; largest one is closer to 999 * 999 than to 100 * 100 :)
(define (largest-palindrome-product-decr a b)
  (cond
   ((palindrome? (* a b)) (list a "*" b "==" (* a b)))
   ((> a 1)
          (largest-palindrome-product-decr (- a 1) b))
   ((> b 1)
          (largest-palindrome-product-decr a (- b 1)))))

;; tests
(newline)
(display (largest-palindrome-product-decr 99 99))  ;; should be 9009; 91 * 99

;; got lucky in the previous one. we'll need to modify
;; the procedure a little bit to use it only with 3-digit
;; numbers (else, it'll return 91 * 999 == 90909):



(define (make-range low high)
  (define (make-range-helper range curr)
    (if (> curr high)
        range
        (begin
          (set! range (append range (list curr)))
          (make-range-helper range (+ curr 1)))))

  (let ((range '()))
    (make-range-helper range low)))

(display (make-range 1 5))


(define (largest-palindrome-product a_lowest a_highest b_lowest b_highest)
  (define (largest-palindrome-helper a_curr b_curr)
    (cond
     ((palindrome? (* a_curr b_curr)) (list a_curr "*" b_curr "==" (* a_curr b_curr)))
     ((> a_curr a_lowest) '())
     ((> b_curr b_lowest) (largest-palindrome-helper (- a_curr 1) b_highest))
     (else (largest-palindrome-helper a_curr (- b_curr 1)))))

  (let ((range_a (reverse (make-range a_lowest a_highest)))
        (range_b (reverse (make-range b_lowest b_highest))))
    (map (lambda (a_val)
           (map (lambda (b_val)
                  (begin
                    (largest-palindrome-helper a_val b_val)))
                range_b))
         range_a)))



(newline)
(display (largest-palindrome-product 100 999 100 999))
