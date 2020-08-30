#lang racket

#|
The sum of all the multiples of 3 or 5 below 1000.
|#

(define (sum-of-multiples limit)
  (if (= limit 0)
      0
      (if (or (= 0 (remainder limit 3))(= 0 (remainder limit 5)))
          (+ limit (sum-of-multiples (- limit 1)))
          (sum-of-multiples (- limit 1)))))

;(sum-of-multiples (sub1 1000))

#|
The sum of the even-valued terms in the Fibonacci sequence that do not exceed 4 million.
|#

(define (fibonacci limit)
  (let loop ([a 1][b 1][n 1])
    (if (= n limit)
        b
        (loop b (+ a b) (+ 1 n)))))

#|too slow
(define (fibonacci-xd n)
  (if (< n 2)
      1
      (+ (fibonacci-xd (- n 1))(fibonacci-xd (- n 2)))))
|#

(define (sum-even-fibo-bellow limit)
  (let loop ([n 1][sum 0])
    (cond
      [(> (fibonacci n) limit) sum]
      [(even? (fibonacci n)) (loop (+ 1 n) (+ (fibonacci n) sum))]
      [(loop (+ 1 n) sum)])))

;(sum-even-fibo-bellow 4000000)

#|
The largest prime factor of the number 600851475143.
|#

(define (largest-factor n)
  (let loop ([iterator 2] [product 1])
    (cond
      [(= n product) (sub1 iterator)]
      [(= 0 (remainder n iterator))(loop (+ 1 iterator)(* product iterator))]
      [else (loop (+ 1 iterator) product)])))

;(largest-factor 600851475143)

#|
The largest palindrome made from the product of two 3-digit numbers.
|#

(define (count-digits n)
  (if (< n 10)
      1
      (+ 1 (count-digits (quotient n 10)))))

(define (reverse-number n)
  (if (< n 10)
      n
      (+ (* (expt 10 (- (count-digits n) 1)) (remainder n 10)) (reverse-number (quotient n 10)))))

(define (check-palindrome n)
  (= n (reverse-number n)))


(define (largest-palindromeq)
  (let loop ([i 100][j 100][result 0])
    (cond
      [(= i j 999) result]
      [(and (check-palindrome (* i j)) (> (* i j) result)) (loop (+ i 1) j (* i j))]
      [(= i 999) (loop 100 (+ j 1) result)]
      [else (loop (+ i 1) j result)])))

;(largest-palindromeq)

;another method

(define (largest-palindrome)
  (define largest 0)
  (for ([i (in-range 100 1000)])
    (for ([j (in-range 100 1000)])
      (when (and (check-palindrome (* i j)) (< largest (* i j)))
        (set! largest (* i j)))))
  largest)

;(largest-palindrome)

#|
The smallest positive number that is evenly divisible by all of the numbers from 1 to 20.
|#

(define (check-prime n)
  (let loop ([iterator 2])
    (cond
      [(> iterator (sqrt n)) #t]
      [(= 0 (remainder n iterator)) #f]
      [else (loop (+ 1 iterator))])))

(define (nth-prime k)
  (let loop ([iterator 1][optimus 3])
    (cond
      [(= iterator k) (- optimus 2)]
      [(check-prime optimus) (loop (+ 1 iterator) (+ 2 optimus))]
      [else (loop iterator (+ 2 optimus))])))

(time (nth-prime 10001))
      