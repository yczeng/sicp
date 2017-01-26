#lang racket
"""Project 2: Catherine (Yue) Zeng 1-19-17"""

;; By default, Racket doesn't have set-car! and set-cdr! functions.  The
;; following line allows us to use those:
(require r5rs)
;; Unfortunately, it does this by making cons return a "mutable pair"
;; instead of a "pair", which means that many built-ins may fail
;; mysteriously because they expect a pair, but get a mutable pair.
;; Re-define a few common functions in terms of car and friends, which
;; the above line make work with mutable pairs.
(define first car)
(define rest cdr)
(define second cadr)
(define third caddr)
(define fourth cadddr)
;; We also tell DrRacket to print mutable pairs using the compact syntax
;; for ordinary pairs.
(print-as-expression #f)
(print-mpair-curly-braces #f)


"Problem 1"
(define (make-table)
  (list 'table)) ;; identifier for table
(define (table? table)
  (if (pair? table)
  (equal? (car table) 'table)
  #f)) ;; returns boolean if type is table
(define (table-put! table key value)
  (if (table? table)
  (set-cdr! table (cons (cons key value) (cdr table)))
  (error "not a table")) 'undefined)
(define (table-has-key? table key)
  (not (not (assoc key (cdr table)))))
(define (table-get table key)
  (cdr (assoc key (cdr table))))


(define my-table (make-table))
(table? my-table) ;; => #t
(table-put! my-table 'ben-bitdiddle 'chocolate) ;; => undefined
(table-put! my-table 'alyssa-p-hacker 'cake) ;; => undefined
(table-has-key? my-table 'ben-bitdiddle) ;; => #t
(table-has-key? my-table 'louis-reasoner) ;; => #f
(table-get my-table 'ben-bitdiddle) ;; => chocolate
;;(table-get my-table 'louis-reasoner) ;; => ERROR

"Problem 2"
(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

(define (make-monitored f)
  (let ((count 0))
    (lambda (n)
      (cond ((equal? n 'how-many-calls?)
             count)

            ((equal? n 'reset-call-count)
             (set! count 0))
            
             (else (set! count (+ 1 count)) (f n))))))
            
(fib 8) ;; => 21
(set! fib (make-monitored fib))
(fib 8) ;; => 21
(fib 'how-many-calls?) ;; => 67
(fib 8) ;; => 21
(fib 'how-many-calls?) ;; => 134
(fib 'reset-call-count)
(fib 'how-many-calls?) ;; => 0
;; make-monitored

"Problem 3"

(define (make-num-calls-table procedure max)
    (let ((mytable (make-table)))
       (define (helper n)
          (procedure 'reset-call-count)
          (procedure n)
          (table-put! mytable n (procedure 'how-many-calls?))
          (if (= n 1)
              mytable
              (helper (- n 1))))
      (helper max)))

(make-num-calls-table fib 20)
(make-num-calls-table fib 30)

;;How many calls to fib are made when you evaluate (fib 20)? (fib 30)?
;; a lot...(20 . 21891) and (30 . 2692537)

"Problem 4"

(define (memoize procedure)
  (let ((tablevalues (make-table)))
    (lambda (n)
      (cond ((table-has-key? tablevalues n)
           (table-get tablevalues n))

            (else (table-put! tablevalues n (procedure n))
                  (procedure n) )))))
      
(set! fib (memoize fib))
(fib 8) ;; => 21
(fib 9) 

"Problem 5 (optional)"

;; advise

"Problem 6 (optional)"

;; make-monitored-with-advice


;; Allow this file to be included from elsewhere, and export all defined
;; functions from it.
(provide (all-defined-out))

