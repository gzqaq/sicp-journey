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
(define (2-4-cons x y)
  (lambda (m) (m x y)))
(define (2-4-car z)
  (z (lambda (p q) p)))
(define (2-4-cdr z)
  (z (lambda (p q) q)))


;; exer 2.5
(define (2-5-cons a b)
  (* (expt 2 a) (expt 3 b)))
(define (2-5-car z)
  (define (car-iter n cnt)
    (if (= (remainder n 2) 0)
        (car-iter (/ n 2) (+ cnt 1))
        cnt))
  (car-iter z 0))
(define (2-5-cdr z)
  (define (cdr-iter n cnt)
    (if (= (remainder n 3) 0)
        (cdr-iter (/ n 3) (+ cnt 1))
        cnt))
  (cdr-iter z 0))


;; exer 2.6
(define 2-6-zero (lambda (f) (lambda (x) x)))
(define (2-6-add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))
(define 2-6-one (lambda (f) (lambda (x) (f x))))
(define 2-6-two (lambda (f) (lambda (x) (f (f x)))))
