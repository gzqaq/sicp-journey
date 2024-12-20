#lang sicp

;; name things with define
(define pi 3.14159)

;; procedure definition
(define (square x) (* x x))
(define (sum-of-squares x y)
  (+ (square x) (square y)))
(define (f a)
  (sum-of-squares (+ a 1) (* a 2)))
