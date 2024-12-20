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
