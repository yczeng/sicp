#lang racket
#Catherine Zeng 1/12/17
#MIT 6.037 Project 0

-
"Part 1: Numerical integration"

"Problem 1: Bitdiddle's function"

(define (bitfunc x)
    (+ (- (expt x 4)(* 5 (expt x 2))) 4))

;; Some simple test cases, based on by-eye examination of a graph of the
;; function: https://www.google.com/search?q=x^4-5*x^2%2B4   Run these,
;; and check that they match with the expectations.
(bitfunc 0)  ;; Should be 4
(bitfunc 1)  ;; Should be 0, or very close
(bitfunc 2)  ;; Should also be very close to 0
(bitfunc -1) ;; Should also also be very close to 0
(bitfunc 10) ;; Should be pretty big, and positive



"Problem 2: A rectangle under Bitdiddle's function"

(define (bitfunc-rect x1 x2)
    (* (bitfunc x1)(- x2 x1)))

;; Test cases:
(bitfunc-rect 0 1)   ;; Should be 4
(bitfunc-rect 0 0.5) ;; Should be 2
(bitfunc-rect 1.5 2) ;; Should be negative


"Problem 3: Integrating Bitdiddle's function"

(define (bitfunc-integral-recur num-steps x1 x2)
    (cond ((= num-steps 1)
    	(bitfunc-rect x1 x2))
    	(else (+ (bitfunc-rect ( + x1 (* (- num-steps 1) (/ (- x2 x1) num-steps))) x2)
     	(bitfunc-integral-recur (- num-steps 1) x1 (+ x1 (* (- num-steps 1) (/ (- x2 x1) num-steps))) )))))

(bitfunc-integral-recur 2 0 1)  ;; Should be 109/32
(bitfunc-integral-recur 2 0 5)  ;; Should be 1265/32
(bitfunc-integral-recur 3 0 6)  ;; Should be 368



(define (helper num-steps x1 x2 sum)
	(if (= num-steps 0)
		sum
		(helper (- num-steps 1) x1 (+ x1 (* (- num-steps 1) (/ (- x2 x1) num-steps))) (+ sum (bitfunc-rect ( + x1 (* (- num-steps 1) (/ (- x2 x1) num-steps))) x2) ))))

(define (bitfunc-integral-iter num-steps x1 x2) (helper num-steps x1 x2 0))

(bitfunc-integral-iter 2 0 1)  ;; Should be 109/32
(bitfunc-integral-recur 2 0 5)  ;; Should be 1265/32
(bitfunc-integral-recur 3 0 6)  ;; Should be 368


"Problem 4: Comparing the two integrators"

(define (bitfunc-integral-difference num-steps x1 x2)
    (abs (- (bitfunc-integral-recur num-steps x1 x2) (bitfunc-integral-iter num-steps x1 x2))))

(bitfunc-integral-difference 2 0 1) ;; Should be 0
(bitfunc-integral-difference 2 0 5) ;; Should be 0
(bitfunc-integral-difference 3 0 6) ;; Should be 0


