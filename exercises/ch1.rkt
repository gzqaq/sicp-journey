#lang sicp

(define exercise-1-2
  (/ (+ 5 4
        (- 2
           (- 3
              (+ 6 (/ 4 5)))))
     (* 3
        (- 6 2)
        (- 2 7))))

(define (square x) (* x x))

(define (sum-of-squares a b)
  (+ (square a) (square b)))

(define exercise-1-3
  (lambda (a b c)
    (cond ((and (<= a b)
                (<= a c))
           (sum-of-squares b c))
          ((and (<= b a)
                (<= b c))
           (sum-of-squares a c))
          (else
           (sum-of-squares a b)))))

(define (exercise-1-7 x)
  (define (sqrt-iter last-guess guess x)
    (if (good-enough? last-guess guess)
        guess
        (sqrt-iter guess (improve guess x) x)))
  (define (improve guess x)
    (average guess (/ x guess)))
  (define (good-enough? last-guess guess)
    (< (abs (- last-guess guess)) 0.001))
  (define (average a b)
    (/ (+ a b) 2))
  (sqrt-iter 0.0 1.0 x))

(define (exercise-1-8 x)
  (define (cube-root-iter last-guess guess x)
    (if (good-enough? last-guess guess)
        guess
        (cube-root-iter guess (improve guess x) x)))
  (define (improve guess x)
    (/ (+ (/ x (square guess)) (* 2 guess)) 3))
  (define (good-enough? last-guess guess)
    (< (abs (- last-guess guess)) 0.001))
  (cube-root-iter 0.0 1.0 x))

(define (exercise-1-11-rec n)
  (if (< n 3)
      n
      (+ (exercise-1-11-rec (- n 1))
         (* 2 (exercise-1-11-rec (- n 2)))
         (* 3 (exercise-1-11-rec (- n 3))))))

(define (exercise-1-11-iter n)
  (define (f--iter a b c counter)
    (if (= counter 0)
        c
        (f--iter (+ a
                    (* 2 b)
                    (* 3 c))
                 a
                 b
                 (- counter 1))))
  (f--iter 2 1 0 n))

(define (exercise-1-12 a b)
  (if (or (= a b)
          (= 1 b))
      1
      (+ (exercise-1-12 (- a 1) (- b 1))
         (exercise-1-12 (- a 1) b))))

(define (exer-1-16 b n)
  (define (expt-iter a b n)
    (cond ((= n 0) a)
          ((even? b) (expt-iter a (square b) (/ n 2)))
          (else (expt-iter (* a b) b (- n 1)))))
  (expt-iter 1 b n))

(define (double n)
  (* n 2))

(define (halve n)  ; divides an even number
  (/ n 2))

(define (exer-1-17 a b)
  (cond ((= b 0) 0)
        ((even? b) (double (exer-1-17 a (halve b))))
        (else (+ a (exer-1-17 a (- b 1))))))

(define (exer-1-18 a b)
  (define (*-iter product a b)
    (cond ((= b 0) product)
          ((even? b) (*-iter product (double a) (halve b)))
          (else (*-iter (+ product a) a (- b 1)))))
  (*-iter 0 a b))

(define (exer-1-19 n)
  (define (fib-iter a b p q counter)
    (cond ((= counter 0) b)
          ((even? counter) (fib-iter a b
                                     (sum-of-squares p q)
                                     (+ (square q) (* 2 p q))
                                     (halve counter)))
          (else (fib-iter (+ (* b q)
                             (* a q)
                             (* a p))
                          (+ (* b p)
                             (* a q))
                          p q
                          (- counter 1)))))
  (fib-iter 1 0 0 1 n))

;; exercise 1.22
(define (timed-prime-test n)
  (define (start-prime-test n start-time)
    (if (prime? n)
        (report-prime (- (runtime) start-time))))
  (define (report-prime elapsed-time)
    (display " *** ")
    (display elapsed-time))
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (search-for-primes beg)
  (define (search-from-odd n)
    (if (prime? n)
        n
        (search-from-odd (+ n 2))))
  (search-from-odd (if (even? beg) (+ 1 beg) beg)))

(define (prime? n)
  (= (smallest-divisor n) n))
;; (define (prime? n)
;;   (fast-prime? n 11))

; prime? by smallest-divisor
(define (smallest-divisor n)
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (+ test-divisor 1)))))
  (define (divides? a b)
    (= (remainder b a) 0))
  (find-divisor n 2))

; probabilistic prime? by Fermat test
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
        (else (remainder (* base (expmod base (- exp 1) m)) m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))
