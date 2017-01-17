#lang racket
;; 1/17/17 Catherine Zeng

"Part 1: Numerical integration"

"Problem 1: Integrating any function"

;; I used an interative approach to get the integral of the function

(define (integral func num-steps x1 x2)
    (define (rect x1 x2)
      (* (func x1)(- x2 x1)))
  (define (helper num-steps x1 x2 sum)
	(if (= num-steps 0)
		sum
		(helper (- num-steps 1) x1 (+ x1 (* (- num-steps 1) (/ (- x2 x1) num-steps))) (+ sum (rect ( + x1 (* (- num-steps 1) (/ (- x2 x1) num-steps))) x2) ))))
  (define (integral-iter num-steps x1 x2) (helper num-steps x1 x2 0))
  (integral-iter num-steps x1 x2))

;; Test cases:

;; With only one step, the integral of y = x^2 from 3 to 5
;; should be 3^2 * 2 = 18
(integral (lambda (x) (expt x 2)) 1 3 5)
;; With two steps, we should get 3^2 + 4^2 = 25
(integral (lambda (x) (expt x 2)) 2 3 5)

"Problem 2: Area of a unit circle"

;; approximates pi using integral from 0 to 1 of sqrt(1-x^2) times 4

(define (approx-pi num-steps)
    (* 4 (integral (lambda (x) (sqrt (- 1 (expt x 2)))) num-steps 0 1)))
(approx-pi 1)   ;; Should be 4
(approx-pi 2)   ;; Hopefully lower than 4
(approx-pi 600) ;; Right to the first two decimal places?

"Problem 3: Integrating with pieces of any shape"

;; here I created definitions for estimating integrals through using both rectangles and trapozoids.

(define (rectangle func x1 x2)
    (* (func x1)(- x2 x1)))

(define (trapezoid func x1 x2)
    (* (/ (+ (func x1)(func x2)) 2) (- x2 x1)))

(define (integral-with piece func num-steps x1 x2)
       (define (integral-recur num-steps x1 x2)
         (if (= num-steps 1)
             (piece func x1 x2)
        
             (+ (piece func ( + x1 (* (- num-steps 1) (/ (- x2 x1) num-steps))) x2)
             (integral-recur (- num-steps 1) x1 (+ x1 (* (- num-steps 1) (/ (- x2 x1) num-steps))))) ) )
  
  (integral-recur num-steps x1 x2))

  
(integral-with rectangle (lambda (x) (expt x 2)) 3 0 3) ;; Should be 5
(integral-with trapezoid (lambda (x) (expt x 2)) 3 0 3) ;; Should be 9.5


"Problem 4: Better approximation of pi"

;; uses trapozoids to estimate integral

(define (better-pi num-steps)
    (* 4 (integral-with trapezoid (lambda (x) (sqrt (- 1 (expt x 2)))) num-steps 0 1)))

(better-pi 1)   ;; Should be 4
(better-pi 2)   ;; Hopefully lower than 4
(better-pi 600) ;; Right to the first two decimal places?


"Part 2: Symbolic differentiation"

(define (deriv-constant wrt constant)
    0)


"Problem 5: Derivative of a variable"

(define (deriv-variable wrt var)
    (if (eq? wrt var)
        1
        0))

(deriv-variable 'x 'x) ;; -> 1
(deriv-variable 'x 'y) ;; -> 0
(deriv-variable 'y 'y) ;; -> 0
(deriv-variable 'm 'y) ;; -> 0

"Problem 6: Calling the right function"

;; derives a function based on the type of expression you put in

(define (derivative wrt expr)
    (cond ((number? expr)
        (deriv-constant wrt expr))
          
        ((symbol? expr)
         (deriv-variable wrt expr))
        
        ((list? expr)
         (cond ((equal? (car (car (list expr))) '+)
                (deriv-sum wrt expr))
               ((equal? (car (car (list expr))) '*)
                (deriv-product wrt expr))
               ))
        (else (error "Don't know how to differentiate" expr))))        

(derivative 'x 'x)
(derivative 'x 'y)
(derivative 'x 6)


"Problem 7: Derivative of a sum"

(define (deriv-sum wrt expr)
    (list '+ (derivative wrt (car (cdr (car (list expr)))))
    (derivative wrt (car (cdr (cdr (car (list expr)))))))
  )

(derivative 'x '(+ x 2)) ; -> (+ 1 0)


"Problem 8: Derivative of a product"

(define (deriv-product wrt expr)
     (list '+ (list '* (car (cdr (car (list expr)))) 0)
    (list '* 1 (car (cdr (cdr (car (list expr)))))))
  )

(derivative 'x '(* y 5)) ; -> (+ (* x 0) (* 1 3))

"Problem 9: Additional testing"

; Additional test cases for 'derivative' go here.

#| output of my program:
"Part 1: Numerical integration"
"Problem 1: Integrating any function"
18
25
"Problem 2: Area of a unit circle"
4
3.732050807568877
3.144845975564729
"Problem 3: Integrating with pieces of any shape"
5
9 1/2
"Problem 4: Better approximation of pi"
2
2.732050807568877
3.1415126422313975
"Part 2: Symbolic differentiation"
"Problem 5: Derivative of a variable"
1
0
1
0
"Problem 6: Calling the right function"
1
0
0
"Problem 7: Derivative of a sum"
(+ 1 0)
"Problem 8: Derivative of a product"
(+ (* y 0) (* 1 5))
"Problem 9: Additional testing"|#
