#lang sicp

;; name things with define
(define pi 3.14159)

;; procedure definition
(define (square x) (* x x))
(define (sum-of-squares x y)
  (+ (square x) (square y)))
(define (f a)
  (sum-of-squares (+ a 1) (* a 2)))

;; case analysis using cond(itional)
(define (abs--cond-1 x)
  (cond ((> x 0) x)  ; clause = (predicate expression)
        ((= x 0) 0)
        ((< x 0) (- x))))

(define (abs--cond-2 x)
  (cond ((< x 0) (- x))
        (else x)))

(define (abs--if x)
  (if (< x 0)
      (- x)
      x))

;; greater than or equal to using logical composition operations
(define (>=--or x y)
  (or (> x y) (= x y)))  ; or, and are special forms

(define (>=--not x y)
  (not (< x y)))  ; not is an ordinary procedure

;; square root by newton's method
(define (sqrt-newton x)
  (sqrt-iter 1.0 x))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

;; block structure and lexical scoping
(define (sqrt-newton-block x)
  (define (good-enough? guess)  ; make x a free variable for these procedures -> lexical scoping
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

;; linear recursive process for computing factorial
(define (factorial-rec n)
  (if (= n 1)
      1
      (* n (factorial-rec (- n 1)))))

;; linear iterative process for computing factorial
(define (factorial-lin n)
  (define (fac-iter product counter)
    (if (> counter n)
        product
        (fac-iter (* product counter) (+ counter 1))))
  (fac-iter 1 1))
; Note that although we use a recursive procedure, this is an iterative process.  And thanks to the
; tail-recursive implementation, this process is executed in constant space, and iteration can be
; expressed using the ordinary procedure call mechanism and special iteration constructs like
; for-loop are useful only as syntactic sugar.

;; Ackermann's function
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

;; tree recursion e.g. fibonacci
(define (fib-rec n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib-rec (- n 1))
                 (fib-rec (- n 2))))))

(define (fib-iter n)
  (define (fib--iter a b counter)
    (if (= counter 0)
        b
        (fib--iter (+ a b) a (- counter 1))))
  (fib--iter 1 0 n))

;; tree-recursive process is natural, whereas an iterative version is a challenge
;; e.g. counting change
(define (count-change amount)
  (define (cc amt kinds-of-coins)
    (cond ((= amt 0) 1)
          ((or (< amt 0)
               (= kinds-of-coins 0))
           0)
          (else
           (+ (cc amt (- kinds-of-coins 1))
              (cc (- amt (first-denomination kinds-of-coins)) kinds-of-coins)))))
  (define (first-denomination kinds-of-coins)
    (cond ((= kinds-of-coins 1) 1)
          ((= kinds-of-coins 2) 5)
          ((= kinds-of-coins 3) 10)
          ((= kinds-of-coins 4) 25)
          ((= kinds-of-coins 5) 50)))
  (cc amount 5))

;; linear recursive version of exponential
(define (expt-rec b n)
  (if (= n 0)
      1
      (* b (expt-rec b (- n 1)))))

(define (expt-iter b n)
  (define (expt--iter product counter)
    (if (= counter 0)
        product
        (expt--iter (* product b) (- counter 1))))
  (expt--iter 1 n))

;; logn fast exponential
(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))
