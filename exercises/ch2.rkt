#lang sicp

;; exer 2.1
(define (make-rat n d)
  (let ((g (gcd n d))
        (denom-sign (if (> d 0) 1 (- 1))))
    (cons (* (/ n g) denom-sign)
          (* (/ d g) denom-sign))))


;; exer 2.2
(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (make-segment x y) (cons x y))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))


;; exer 2.4
(define (exer-2-4-cons x y)
  (lambda (m) (m x y)))
(define (exer-2-4-car z)
  (z (lambda (p q) p)))
(define (exer-2-4-cdr z)
  (z (lambda (p q) q)))
