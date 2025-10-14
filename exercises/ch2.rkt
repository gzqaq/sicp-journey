#lang sicp

;; utilities
(define (square n) (* n n))

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


;; exer 2.17
(define (last-pair items)
  (if (null? (cdr items))
      items
      (last-pair (cdr items))))


;; exer 2.18
(define (reverse items)
  (define (rev-iter items res)
    (if (null? items)
        res
        (rev-iter (cdr items) (cons (car items) res))))
  (rev-iter items nil))


;; exer 2.19
(define (count-change amount coins)
  (define (cc amt coin-values)
    (cond ((= amt 0) 1)
          ((or (< amt 0)
               (no-more? coin-values))
           0)
          (else
           (+ (cc amt (except-first-denomination coin-values))
              (cc (- amt (first-denomination coin-values)) coin-values)))))
  (define (first-denomination coin-values)
    (car coin-values))
  (define (except-first-denomination coin-values)
    (cdr coin-values))
  (define (no-more? coin-values)
    (null? coin-values))
  (cc amount coins))

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))


;; exer 2.20
(define (same-parity n . integers)
  (let ((parity (remainder n 2)))
    (define (has-parity-inner seq result)
      (cond ((null? seq) result)
            ((= parity (remainder (car seq) 2))
             (has-parity-inner (cdr seq) (cons (car seq) result)))
            (else (has-parity-inner (cdr seq) result))))
    (reverse (has-parity-inner integers nil))))


;; exer 2.21
(define (square-list items)
  (if (null? items)
      nil
      (cons (square (car items)) (square-list (cdr items)))))

(define (square-list-map items)
  (map square items))


;; exer 2.23
(define (zq/for-each func items)
  (define (step items)
    (func (car items))
    (cdr items))
  (if (null? items)
      nil
      (zq/for-each func (step items))))

(define (zq/for-each-map func items)
  (map func items)
  nil)


;; exer 2.27
(define (deep-reverse x)
  (define (rev-iter result t)
    (cond ((null? t) result)
          ((not (pair? t)) t)
          (else (rev-iter (cons (rev-iter nil (car t)) result) (cdr t)))))
  (rev-iter nil x))


;; exer 2.28
(define (fringe x)
  (define (fringe-iter result t)
    (cond ((null? t) result)
          ((not (pair? t)) (cons t result))
          (else (fringe-iter (fringe-iter result (car t)) (cdr t)))))
  (reverse (fringe-iter nil x)))


;; exer 2.29
(define (make-mobile left right) (list left right))
(define (make-branch len structure) (list len structure))

(define (left-branch x) (car x))
(define (right-branch x) (cadr x))
(define (branch-length b) (car b))
(define (branch-structure b) (cadr b))

(define e2.29-input (make-mobile (make-branch 6 (make-mobile (make-branch 3 4.0)
                                                             (make-branch 9 (make-mobile
                                                                             (make-branch 1 3.0)
                                                                             (make-branch 9 0.1)))))
                                 (make-branch 77 -4.0)))
(define (branch? x) (not (pair? (car x))))

(define (total-weight x)
  (if (not (pair? x))
      x
      (+ (total-weight (branch-structure (left-branch x)))
         (total-weight (branch-structure (right-branch x))))))

(define e2.29-balanced-mobile
  (make-mobile (make-branch 6 (make-mobile (make-branch 2 (make-mobile (make-branch 1 2)
                                                                       (make-branch 2 1)))
                                           (make-branch 3 2)))
               (make-branch 2 (make-mobile (make-branch 2 5) (make-branch 1 10)))))

(define (balanced? x)
    (if (not (pair? x))
        true
        (let ((lb (left-branch x))
              (rb (right-branch x)))
          (and (= (* (branch-length lb) (total-weight (branch-structure lb)))
                  (* (branch-length rb) (total-weight (branch-structure rb))))
               (balanced? (branch-structure lb))
               (balanced? (branch-structure rb))))))
