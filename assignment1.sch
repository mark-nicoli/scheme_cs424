#lang racket
(define ttms-eval
  (λ (e v)
    (cond ((number? e) e)
	  ((equal? e 'x) v)
	  (else
	   (let ((op (car e))
		 (args (cdr e)))
	     (apply (lookup op ttml-op-table)
		    (map (λ (ee) (ttms-eval ee v)) args)))))))

(define ttml-op-table
  (list (list '+ (λ (x1 x2) (+ x1 x2)))
	(list '* (λ (x1 x2) (* x1 x2)))
	(list 'sin sin)
	(list 'cos cos)
	(list 'exp exp)))

;;; Alt definition, using pre-defined function assoc:

(define lookup (λ (x alist) (cadr (assoc x alist))))

(define d
  (λ (e)
    (cond ((number? e) 0)
	  ((equal? e 'x) 1)
	  (else
	   ;; We handle only BINARY ops here, and only + and *
	   (ttms-eval (let ((op (car e)) (u (cadr e)) (v (caddr e)))
	     (cond ((equal? op '+)
		    (list '+ (d u) (d v)))
		   ((equal? op '*)
		    (list '+
			  (list '* u (d v))
			  (list '* (d u) v)))
		   (else (error))))1)))))
