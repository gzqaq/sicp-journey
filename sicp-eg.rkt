#lang sicp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Chapter 1: Building Abstractions with Procedures ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;; gcd by euclid's algorithm
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;; prime? by smallest-divisor
(define (smallest-divisor n)
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (+ test-divisor 1)))))
  (define (divides? a b)
    (= (remainder b a) 0))
  (find-divisor n 2))

(define (prime? n)
  (= (smallest-divisor n) n))

;; probabilistic prime? by Fermat test
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

;; a higher-order procedure, sum
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

;; various sum
(define (cube n) (* n n n))

(define (sum-cubes a b)
  (sum cube a inc b))

(define (sum-integers a b)
  (sum identity a inc b))

; pi = (* 8 (pi-sum 1 1000000...))
(define (pi-sum a b)
  (define (pi-term x) (/ 1.0 (* x (+ x 2))))
  (define (pi-next x) (+ x 4))
  (sum pi-term a pi-next b))

;; integral by sum
(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

;; pi-sum and integral by lambda
(define (pi-sum-lambda a b)
  (sum (lambda (x) (/ 1.0 (* x (+ x 2))))
       a
       (lambda (x) (+ x 4))
       b))

(define (integral-lambda f a b dx)
  (* (sum f (+ a (/ dx 2.0))
          (lambda (x) (+ x dx))
          b)
     dx))

;; use lambda to create local variables and a corresponding grammar sugar--let
(define (f-lambda x y)
  ((lambda (a b) (+ (* x (square a))
                    (* y b)
                    (* a b)))
   (+ 1 (* x y))
   (- 1 y)))

(define (f-let x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))

;; example: half-interval method
(define (search f neg-point pos-point)
  (let ((mid-point (average neg-point pos-point)))
    (if (close? neg-point pos-point)
        mid-point
        (let ((test-value (f mid-point)))
          (cond ((positive? test-value)
                 (search f neg-point mid-point))
                ((negative? test-value)
                 (search f mid-point pos-point))
                (else mid-point))))))

(define (close? x y)  ;; numpy.isclose
  (let ((rtol 0.00001)
        (atol 0.00000001))
    (<= (abs (- x y))
        (+ atol (* rtol (abs y))))))

(define (half-interval-method f a b)
  (let ((f-a (f a))
        (f-b (f b)))
    (cond ((and (negative? f-a)
                (positive? f-b))
           (search f a b))
          ((and (positive? f-a)
                (negative? f-b))
           (search f b a))
          (else
           (error "Values are not of opposite sign" a b)))))

;; example: find fixed-point
(define (fixed-point f first-guess)
  (define (try guess)
    (let ((next-guess (f guess)))
      (if (close? next-guess guess)
          next-guess
          (try next-guess))))
  (try first-guess))

;; finding a y s.t. y^2 = x is equiv. to finding fixed point of y -> x / y, or y -> (y + x / y) / 2,
;; since next-guess is again y when guess is x / y
(define (sqrt-fixed-point x)
  (fixed-point (lambda (y) (average (/ x y) y))
               1.0))

;; average damping
(define (average-damp f)
  (lambda (x)
    (average x (f x))))

(define (sqrt-aver-damp x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))

(define (cube-root-aver-damp x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
               1.0))

;; newton's method: solution to g(x) = 0 is a fixed point of x - g(x) / dg(x)
(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define dx 0.00001)

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (sqrt-fixed-newton x)
  (newtons-method (lambda (y) (- (square y) x)) 1.0))

;; each sqrt begins with a function and finds a fixed point of some transformation of the function
(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (sqrt-damp-transform x)
  (fixed-point-of-transform
   (lambda (y) (/ x y))
   average-damp
   1.0))

(define (sqrt-newton-transform x)
  (fixed-point-of-transform
   (lambda (y) (- (square y) x))
   newton-transform
   1.0))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Chapter 2: Building Abstractions with Data ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; assuming we have the constructor `make-rat' and selectors `numer' and `denom'
(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

;; The single compound-data primitive "pair", implemented by `cons', `car', and `cdr', is the only
;; glue we need to construct data objects, which will be called "list-structred" data.
(define (make-rat--raw n d) (cons n d))
(define (numer x) (car x))
(define (denom x) (cdr x))

;; pretty print
(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

;; use `gcd' to reduce the numerator and denominator to lowest terms before constructing the pair
(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

;; any triple of procedures that satisfies the condition for `cons', `car' and `cdr' can be used as
;; the basis for implementing pairs
(define (my-cons x y)
  (define (dispatch m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else (error "Argument not 0 or 1 -- CONS" m))))
  dispatch)

(define (my-car z) (z 0))
(define (my-cdr z) (z 1))

;; list-ref
(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

;; length of list
(define (length--rec items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))
(define (length--iter items)
  (define (length-iter a cnt)
    (if (null? a)
        cnt
        (length-iter (cdr a) (+ cnt 1))))
  (length-iter items 0))

;; append a list to another
(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

;; the number of leaves of a tree represented by a nested list
(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))
