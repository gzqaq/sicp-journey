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
