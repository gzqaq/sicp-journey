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
