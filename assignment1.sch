;;;  - The d function should return ttms expressions that are in more simplified form.
;;;  - Both ttms-eval and d should handle a larger set of operators.
;;;    - Unary: sin, cos, exp, recip (1/x), log (natural log)
;;;    - Binary: +, *, / (x/y), expt (x^y)
;;;  - Both ttms-eval and d should be table-driven.



#lang racket
(define d
  (λ (e)
    (cond ((number? e) 0)
	  ((equal? e 'x) 1)
	  (else
	   (let ((op (car e)))
	     (let ((func (lookup op deriv-table)))
	       (apply func
		      (append (cdr e)
			      (map d (cdr e))))))))))

(define ttms-op-table
  (list (list '+ (λ (v1 v2) (+ v1 v2)))
	(list '* (λ (v1 v2) (* v1 v2)))
	(list '- (λ (v) (- v)))
        (list '/ (λ (v1 v2) (/ v1 v2)))
	(list 'sin sin)
	(list 'cos cos)
        (list 'log log) 
        (list 'exp exp)
        (list 'recip (λ (x1) (/ 1 x1)))))

(define deriv-table
  (list (list '+ (λ (u v du dv) (ttms_add du dv)))
	(list '* (λ (u v du dv) (ttms_add (ttms_mul u dv) (ttms_mul du v))))
	(list '- (λ (u du) (ttms_sub du)))
        (list '/ (λ (u v du dv) (ttms_sub (ttms_mul v du) (ttms_mul u dv))))
	(list 'sin (λ (u du) (ttms_mul (ttms-cos u) du)))
	(list 'cos (λ (u du) (ttms_sub (ttms_mul (ttms-sin u) du))))
        (list 'exp (λ (u du) (u)))))

(define lookup
  (λ (key table)
    (cond ((equal? key (caar table))
	  (cadr (car table)))
	  (else (lookup key (cdr table))))))

(define ttms-cos (λ (e) (list 'cos e)))

(define ttms-sin (λ (e) (list 'sin e)))

(define ttms_add
  (λ (u v)
    (cond ((equal? u 0) v)((equal? v 0) u)((and (number? u) (number? v)) (+ u v))
	  (else (list '+ u v)))))

(define ttms_mul
  (λ (u v)
    (cond ((equal? u 1) v)((equal? v 1) u)((equal? u 0) 0)((equal? v 0) 0)((and (number? u) (number? v)) (* u v))
	  (else (list '* u v)))))

(define ttms_sub
  (λ (u)
    (cond ((number? u) (- u))
	  (else (list '- u)))))

(define ttms_div
  (λ (u v)
    (cond ((and (number? u) (number? v)) (/ u v))
          (else (list '/ u v)))))


(define ttms-eval
  (λ (e x)
    (cond ((number? e) e)((equal? e 'x) x)
	  (else (let ((f (lookup (car e) ttms-op-table)))
		  (apply f (map (λ (e1) (ttms-eval e1 x)) (cdr e))))))))


;;; test cases

(d '(* (+ x 1) (+ x -1)))

(d '(* x (* x (* x (* x x)))))

(d '(+ (sin (+ x 6)) (* x 9))) ;;; --> (+ (cos (+ x 6)) 9)

(d '(+ (+ 1 9) (+ 3 8)))
