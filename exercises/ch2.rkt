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


;; exer 2.7-12
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))
;; (define (mul-interval x y)
;;   (let ((p1 (* (lower-bound x) (lower-bound y)))
;;         (p2 (* (lower-bound x) (upper-bound y)))
;;         (p3 (* (upper-bound x) (lower-bound y)))
;;         (p4 (* (upper-bound x) (upper-bound y))))
;;     (make-interval (min p1 p2 p3 p4)
;;                    (max p1 p2 p3 p4))))
;; (define (div-interval x y)
;;   (mul-interval x
;;                 (make-interval (/ 1.0 (upper-bound y))
;;                                (/ 1.0 (lower-bound y)))))

(define (make-interval a b) (cons a b))
(define (lower-bound x) (car x))
(define (upper-bound x) (cdr x))

(define (sub-interval x y)
  (add-interval x (make-interval (- (upper-bound y))
                                 (- (lower-bound y)))))

(define (div-interval x y)
  (if (> (* (lower-bound y) (upper-bound y)) 0)
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))
      (error "Divide by an interval that spans zero" y)))

(define (mul-interval x y)
  (let ((x-l (lower-bound x))
        (x-u (upper-bound x))
        (y-l (lower-bound y))
        (y-u (upper-bound y)))
    (cond ((and (>= x-l 0)
                (>= y-l 0))
           (make-interval (* x-l y-l) (* x-u y-u)))
          ((and (>= x-u 0)
                (>= y-l 0))
           (make-interval (* x-l y-u) (* x-u y-u)))
          ((>= y-l 0)
           (make-interval (* x-l y-u) (* x-u y-l)))
          ((and (>= x-l 0)
                (>= y-u 0))
           (make-interval (* x-u y-l) (* x-u y-u)))
          ((and (>= x-u 0)
                (>= y-u 0))
           (make-interval (min (* x-u y-l)
                               (* x-l y-u))
                          (max (* x-u y-u)
                               (* x-l y-l))))
          ((>= y-u 0)
           (make-interval (* x-l y-u) (* x-l y-l)))
          ((>= x-l 0)
           (make-interval (* x-u y-l) (* x-l y-u)))
          ((>= x-u 0)
           (make-interval (* x-u y-l) (* x-l y-l)))
          (else
           (make-interval (* x-u y-u) (* x-l y-l))))))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percentage c p)
  (make-center-width c (* c p)))
(define (percent i)
  (/ (width i) i))
