#lang racket
;; keberos: yczeng
;; Catherine Zeng 1/23/17
;; Worked with Meryl Wang (on questions 1 and 2)
;;
;; Used a combination of harassing instructors, googling,
;; and banging head on wall for others
;;
;; eval.scm - 6.037
;;
(require r5rs)
(define first car)
(define second cadr)
(define third caddr)
(define fourth cadddr)
(define rest cdr)
;; Tell DrRacket to print mutable pairs using the compact syntax for
;; ordinary pairs.
(print-as-expression #f)
(print-mpair-curly-braces #f)

;; mutable cons cell version of map
(define (mmap f lst)
  (if (null? lst)
      '()
      (cons (f (car lst)) (mmap f (cdr lst)))))

(define (tagged-list? exp tag)
  (and (pair? exp) (eq? (car exp) tag)))

(define (self-evaluating? exp)
  (cond ((number? exp) #t)
        ((string? exp) #t)
        ((boolean? exp) #t)
        (else #f)))

(define (quoted? exp) (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))

(define (variable? exp) (symbol? exp))
(define (assignment? exp) (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))
(define (make-assignment var expr)
  (list 'set! var expr))

(define (definition? exp) (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))   (cadr exp)   (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp) (cddr exp))))  ; formal params, body
(define (make-define var expr)
  (list 'define var expr))

(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters lambda-exp) (cadr lambda-exp))
(define (lambda-body lambda-exp) (cddr lambda-exp))
(define (make-lambda parms body) (cons 'lambda (cons parms body)))

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp) (cadddr exp))
(define (make-if pred conseq alt) (list 'if pred conseq alt))

(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define first-cond-clause car)
(define rest-cond-clauses cdr)
(define (make-cond seq) (cons 'cond seq))

(define (let? expr) (tagged-list? expr 'let))
(define (let-bound-variables expr) (mmap first (second expr)))
(define (let-values expr) (mmap second (second expr)))
(define (let-body expr) (cddr expr)) ;differs from lecture--body may be a sequence
(define (make-let bindings body)
  (cons 'let (cons bindings body)))

(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions begin-exp) (cdr begin-exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))
(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin exp) (cons 'begin exp))

(define (application? exp) (pair? exp))
(define (operator app) (car app))
(define (operands app) (cdr app))
(define (no-operands? args) (null? args))
(define (first-operand args) (car args))
(define (rest-operands args) (cdr args))
(define (make-application rator rands)
  (cons rator rands))

(define (time? exp) (tagged-list? exp 'time))

;;;=== QUESTION PROBELM 2===
(define (and? expr) (tagged-list? expr 'and))
(define (and-clauses exp) (cdr exp))

;;;=== QUESTION PROBLEM 3===
(define (until? expr) (tagged-list? expr 'until))

;;;=== QUESTION PROBLEM 4===
(define (unset? exp) (tagged-list? exp 'unset!))

;; === QUESTION PROBLEM 5===
(define (procedure-env? exp) (tagged-list? exp 'procedure-env))

;;
;; this section is the actual implementation of meval
;;

(define (m-eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp) (lambda-body exp) env))
        ((begin? exp) (eval-sequence (begin-actions exp) env))
        ((cond? exp) (m-eval (cond->if exp) env))
        ((let? exp) (m-eval (let->application exp) env))
        ((time? exp) (time (m-eval (second exp) env)))
        ;; ==== QUESTION 2 ====
        ((and? exp) (eval-and exp env))
        ;; ==== QUESTION 3 ====
        ((until? exp) (m-eval (until->transformed exp) env))
        ;; ==== QUESTION 4 ====
        ((unset? exp) (eval-unset exp env))
        ;; ==== QUESTION 5 ====
        ((procedure-env? exp) (eval-procedure-env exp env))
   
        ((application? exp)
         (m-apply (m-eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else (error "Unknown expression type -- EVAL" exp))))

(define (m-apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment (make-frame (procedure-parameters procedure)
                                          arguments)
                              (procedure-environment procedure))))
        (else (error "Unknown procedure type -- APPLY" procedure))))

(define (list-of-values exps env)
  (cond ((no-operands? exps) '())
        (else (cons (m-eval (first-operand exps) env)
                    (list-of-values (rest-operands exps) env)))))

(define (eval-if exp env)
  (if (m-eval (if-predicate exp) env)
      (m-eval (if-consequent exp) env)
      (m-eval (if-alternative exp) env)
      ))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (m-eval (first-exp exps) env))
        (else (m-eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (m-eval (assignment-value exp) env)
                       env))

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (m-eval (definition-value exp) env)
                    env))

(define (let->application expr)
  (let ((names (let-bound-variables expr))
        (values (let-values expr))
        (body (let-body expr)))
    (make-application (make-lambda names body)
                      values)))

(define (cond->if expr)
  (let ((clauses (cond-clauses expr)))
    (if (null? clauses)
        #f
        (if (eq? (car (first-cond-clause clauses)) 'else)
            (sequence->exp (cdr (first-cond-clause clauses)))
            (make-if (car (first-cond-clause clauses))
                     (sequence->exp (cdr (first-cond-clause clauses)))
                     (make-cond (rest-cond-clauses clauses)))))))

(define input-prompt ";;; M-Eval input level ")
(define output-prompt ";;; M-Eval value:")

(define (driver-loop) (repl #f))

(define (repl port)
  (if port #f (prompt-for-input input-prompt))
  (let ((input (if port (read port) (read))))
    (cond ((eof-object? input)   'meval-done)
          ((eq? input '**quit**) 'meval-done)
          (else
           (let ((output (m-eval input the-global-environment)))
             (if port #f (begin
                           (announce-output output-prompt)
                           (pretty-display output)))
             (repl port))))))

(define (prompt-for-input string)
  (newline) (newline) (display string) (display meval-depth) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

;; ==== QUESTION 2 ====
;; if there is one #f, expression returns false.
;; loops until returns #f or returns last expression

(define (eval-and exp env)
  (eval-and-helper (and-clauses exp) env))

(define (eval-and-helper exp env)
  (cond ((null? exp)
      #t)

        ((not (m-eval (car exp) env))
         #f)

        ((= (length exp) 1)
         (m-eval (car exp) env))
          
       (else (eval-and-helper (cdr exp) env))))



;; ==== QUESTION 3 ====

;; returns transformed expression that includes a loop function that loops to excute body

(define (until->transformed expr)
  (let ((test (cadr expr))
  	(body (cddr expr)))
    `(let ()
       (define (loop)
     (if ,test
  	'end
  	(begin
          ,@body
                (loop))))
    (loop))))


;; ==== QUESTION 4 ====

(define (eval-unset exp env)
  (unset-variable-value! (cadr exp) env))

;; === QUESTION 5 ===

(define (eval-procedure-env exp env)
  (procedure-environment
   (binding-value
    (find-in-environment (cadr exp) env))))

;; 
;;
;;
;; implementation of meval environment model
;;

; double bubbles
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define (compound-procedure? proc)
  (tagged-list? proc 'procedure))
(define (procedure-parameters proc) (second proc))
(define (procedure-body proc) (third proc))
(define (procedure-environment proc) (fourth proc))


; bindings
(define (make-binding var val)
  (list 'binding var val))
(define (binding? b)
  (tagged-list? b 'binding))
(define (binding-variable binding)
  (if (binding? binding)
      (second binding)
      (error "Not a binding: " binding)))
(define (binding-value binding)
  (if (binding? binding)
      (third binding)
      (error "Not a binding: " binding)))
(define (set-binding-value! binding val)
  (if (binding? binding)
      (set-car! (cddr binding) val)
      (error "Not a binding: " binding)))

; frames
(define (make-frame variables values)
  (define (make-frame-bindings rest-vars rest-vals)
    (cond ((and (null? rest-vars) (null? rest-vals))
           '())
          ((null? rest-vars)
           (error "Too many args supplied" variables values))
          ((symbol? rest-vars)
           (list (make-binding rest-vars rest-vals)))
          ((null? rest-vals)
           (error "Too few args supplied" variables values))
          (else
           (cons (make-binding (car rest-vars) (car rest-vals))
                 (make-frame-bindings (cdr rest-vars) (cdr rest-vals))))))
  (make-frame-from-bindings (make-frame-bindings variables values)))

(define (make-frame-from-bindings list-of-bindings)
  (cons 'frame list-of-bindings))

(define (frame? frame)
  (tagged-list? frame 'frame))
(define (frame-variables frame)
  (if (frame? frame)
      (mmap binding-variable (cdr frame))
      (error "Not a frame: " frame)))
(define (frame-values frame)
  (if (frame? frame)
      (mmap binding-value (cdr frame))
      (error "Not a frame: " frame)))
(define (add-binding-to-frame! binding frame)
  (if (frame? frame)
      (if (binding? binding)
          (set-cdr! frame (cons binding (cdr frame)))
          (error "Not a binding: " binding))
      (error "Not a frame: " frame)))
(define (find-in-frame var frame)
  (define (search-helper var bindings)
    (if (null? bindings)
        #f
        (if (eq? var (binding-variable (first bindings)))
            (first bindings)
            (search-helper var (rest bindings)))))
  (if (frame? frame)
      (search-helper var (cdr frame))
      (error "Not a frame: " frame)))

; environments
(define the-empty-environment '(environment))
(define (extend-environment frame base-env)
  (if (environment? base-env)
      (if (frame? frame)
          (list 'environment frame base-env)
          (error "Not a frame: " frame))
      (error "Not an environment: " base-env)))
(define (environment? env)
  (tagged-list? env 'environment))
(define (enclosing-environment env)
  (if (environment? env)
      (if (eq? the-empty-environment env)
          (error "No enclosing environment of the empty environment")
          (third env))
      (error "Not an environment: " env)))
(define (environment-first-frame env)
  (if (environment? env)
      (second env)
      (error "Not an environment: " env)))
(define (find-in-environment var env)
  (if (eq? env the-empty-environment)
      #f
      (let ((frame (environment-first-frame env)))
        (let ((binding (find-in-frame var frame)))
          (if binding
              binding
              (find-in-environment var (enclosing-environment env)))))))


; name rule
(define (lookup-variable-value var env)
  (let ((binding (find-in-environment var env)))
    (if binding
        (binding-value binding)
        (error "Unbound variable -- LOOKUP" var))))

(define (set-variable-value! var val env)
  (let ((binding (find-in-environment var env)))
    (if binding
        ;; === QUESTION 4 ===
        ;changed set-binding-variable to add-binding-variable
        (add-binding-value! binding val)
        (error "Unbound variable -- SET" var))))

;; === QUESTION 4 ===
;; adds new valuable to binding of variable
(define (add-binding-value! binding val)
	(if (binding? binding)
		(set-cdr! (cdr binding) (cons val (cddr binding)))
		(error "Not a binding: " binding)))

(define (remove-binding-value! binding)
	(if (binding? binding)
		(if (<= 4 (length binding))
			(set-cdr! (cdr binding) (cdddr binding)))
		(error "Not a binding: " binding)))

(define (unset-variable-value! var env)
	(let ((binding (find-in-environment var env)))
		(if binding
			(remove-binding-value! binding)
			(error "Unbound variable -- SET" var))))

;; === END OF QUESTION 4 ===

(define (define-variable! var val env)
  (let ((frame (environment-first-frame env)))
    (let ((binding (find-in-frame var frame)))
      (if binding
          (set-binding-value! binding val)
          (add-binding-to-frame!
           (make-binding var val)
           frame)))))

;; === QUESTION 5 ===

(define (env-variables env)
  (frame-variables (environment-first-frame env)))

(define (env-parent env)
  (enclosing-environment env))

(define (env-value-helper binding) ;; returns false if there is no symbol in environment
  (if (binding? binding)
      (third binding)
      #f))

(define (env-value var env)
  (env-value-helper (find-in-environment var env)))

; primitives procedures - hooks to underlying Scheme procs
(define (make-primitive-procedure implementation)
  (list 'primitive implementation))
(define (primitive-procedure? proc) (tagged-list? proc 'primitive))
(define (primitive-implementation proc) (cadr proc))
(define (primitive-procedures)
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'set-car! set-car!)
        (list 'set-cdr! set-cdr!)
        (list 'null? null?)
        (list '+ +)
        (list '- -)
        (list '< <)
        (list '> >)
        (list '= =)
        (list 'display display)
        (list 'not not)
        ; ... more primitives
        ;; ==== QUESTION 1 ====
        (list '* *)
        (list '/ /)
        (list 'list list)
        (list 'cadr cadr)
        (list 'cddr cddr)
        (list 'newline newline)
        (list 'printf printf)
        (list 'length length)
        (list 'pair? pair?)
        (list 'append append)
        (list 'env-value env-value)
        (list 'env-variables env-variables)
        (list 'env-parent env-parent)
        ;;===QUESTION 6===
        (list 'caddr caddr)
        (list 'cadddr cadddr)
        (list 'symbol? symbol?)
        (list 'eq? eq?)
        (list 'number? number?)
        (list 'string? string?)
        (list 'boolean? boolean?)
        
        ))

(define (primitive-procedure-names) (mmap car (primitive-procedures)))

(define (primitive-procedure-objects)
  (mmap make-primitive-procedure (mmap cadr (primitive-procedures))))

(define (apply-primitive-procedure proc args)
  (apply (primitive-implementation proc) args))

; used to initialize the environment
(define (setup-environment)
  (extend-environment (make-frame (primitive-procedure-names)
                                  (primitive-procedure-objects))
                      the-empty-environment))

(define the-global-environment (setup-environment))



;;;;;;;; Code necessary for question 6
;;
;; This section doesn't contain any user-servicable parts -- you
;; shouldn't need to edit it for any of the questions on the project,
;; including question 5.  However, if you're curious, comments provide a
;; rough outline of what it does.

;; Keep track of what depth we are into nesting
(define meval-depth 1)

;; These procedures are needed to make it possible to run inside meval
(define additional-primitives
  (list (list 'eof-object?      eof-object?)
        (list 'read             read)
        (list 'read-line        read-line)
        (list 'open-input-file  open-input-file)
        (list 'this-expression-file-name
                    (lambda () (this-expression-file-name)))
        (list 'pretty-display   pretty-display)
        (list 'error            error)
        (list 'apply            m-apply))) ;; <-- This line is somewhat interesting
(define stubs
  '(require r5rs mzlib/etc print-as-expression print-mpair-curly-braces))
(define additional-names (mmap first additional-primitives))
(define additional-values (mmap make-primitive-procedure
                               (mmap second additional-primitives)))

(require mzlib/etc)
(define (load-meval-defs)
  ;; Jam some additional bootstrapping structures into the global
  ;; environment
  (set! the-global-environment
        (extend-environment
         (make-frame stubs
                     (mmap (lambda (name)
                             (m-eval '(lambda (x) x) the-global-environment)) stubs))
         (extend-environment
          (make-frame additional-names
                      additional-values)
          the-global-environment)))
  ;; Open this file for reading
  (let ((stream (open-input-file (this-expression-file-name))))
    (read-line stream) ;; strip off "#lang racket" line
    (repl stream))     ;; feed the rest of the definitions into meval

  ;; Update the meval-depth variable inside the environment we're simulating
  (set-variable-value! 'meval-depth (+ meval-depth 1) the-global-environment)
  'loaded)

;;test cases
;output from running test code:

;;=== QUESTION 1===
;;; M-Eval input level 1
;(and (< x 5) (= 6 6) (pair? '
;())).Unbound variable -- LOOKUP x

;;; M-Eval input level 1

;;; M-Eval value:
;#f

;;; M-Eval input level 1
;(and #t #t #f)

;;; M-Eval value:
;#f

;;; M-Eval input level 1
;(and #t #f #t)

;;; M-Eval value:
;#f

;;; M-Eval input level 1
;(and #t #t #t)

;;; M-Eval value:
;#t

;;; M-Eval input level 1
;(and #f #f #f)

;;; M-Eval value:
;#f

;;=== QUESTION 2 ===

;;TEST 1
;; M-Eval input level 1
;;(define x 0)
;;(until (> x 5)
;;(printf "hi")
;;(set! x (+ x 1)))

;; M-Eval value:
;;#<void>
;; M-Eval input level 1
;;hihihihihihi
;; M-Eval value:
;;end

;;TEST 2
;;M-Eval input level 1
;;(define x 0)
;;(until (> x 5)
;;(set! x (+ x 1)))

;; M-Eval value:
;;#<void>
;; M-Eval input level 1
;; M-Eval value:
;;end

;;QUESTION 4===

;;; M-Eval input level 1
;(define x 0)(set! x 12341)(set! x 2342)(set! x 342)(unset! x)

;;; M-Eval input level 1
;x
;;; M-Eval value:
;2342

;;;QUESTION 5===

;;; TEST

;;; M-Eval input level 1
;(define (make-counter)
;(let ((n 0))
;(lambda ()
;(set! n (+ n 1))
;n)))
;(define c (make-counter))
;(c) ;; => 1
;(c) ;; => 2

;;; M-Eval value:
;#<void>
;;; M-Eval input level 1
;;; M-Eval value:
;#<void>

;;; M-Eval input level 1
;;; M-Eval value:
;1
;;; M-Eval input level 1
;;; M-Eval value:
;2

;;; M-Eval input level 1
;(env-value 'n (procedure-env c)) ;; => 2

;;; M-Eval value:
;2

;;===QUESTION 6===
;> (load-meval-defs)
;loaded
;> (driver-loop)

;;; M-Eval input level 1
;(driver-loop)

;;; M-Eval input level 2
;(+ 17 42)

;;; M-Eval value:
;59

;;; M-Eval input level 2
;**quit**

;;; M-Eval value:
;meval-done

;;; M-Eval input level 1
;**quit**
;meval-done
;> 
