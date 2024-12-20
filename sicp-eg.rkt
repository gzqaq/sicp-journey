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
