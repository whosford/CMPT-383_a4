;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;This nlist function comes from the Introduction to Scheme notes posted online for this course
;http://www.cs.sfu.ca/CourseCentral/383/tjd/scheme-intro.html
(define nlist
    (lambda (n lst)
        (and (list? lst) (= n (length lst)))
    )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define is-addition?
	(lambda (expr)
		(and (nlist 3 expr) (equal? '+ (second expr)))
	)
)

(define is-subtraction?
	(lambda (expr)
		(and (nlist 3 expr) (equal? '- (second expr)))
	)
)

(define is-multiplication?
	(lambda (expr)
		(and (nlist 3 expr) (equal? '* (second expr)))
	)
)

(define is-division?
	(lambda (expr)
		(and (nlist 3 expr) (equal? '/ (second expr)))
	)
)

(define is-exponent?
	(lambda (expr)
		(and (nlist 3 expr) (equal? '** (second expr)))
	)
)

(define is-increment?
	(lambda (expr)
		(and (nlist 2 expr) (equal? 'inc (first expr)))
	)
)

(define is-decrement?
	(lambda (expr)
		(and (nlist 2 expr) (equal? 'dec (first expr)))
	)
)

(define simplify 
	(lambda (expr)
		(cond
			((or (number? expr) (symbol? expr)) expr)
			((is-addition? expr)
				(let ((left (simplify (first expr))) (op '+) (right (simplify (third expr))))
					(cond 
						((equal? 0 left) right)
						((equal? 0 right) left)
						(else (list left op right))
					)
				)
			)
			((is-subtraction? expr)
				(let ((left (simplify (first expr))) (op '-) (right (simplify (third expr))))
					(cond
						((equal? left right) 0)
						((equal? 0 left) right)
						((equal? 0 right) left)
						(else (list left op right))
					)
				)
			)
			((is-multiplication? expr)
				(let ((left (simplify (first expr))) (op '*) (right (simplify (third expr))))
					(cond
						((or (equal? 0 left) (equal? 0 right)) 0)
						((equal? 1 left) right)
						((equal? 1 right) left)
						(else (list left op right))
					)
				)
			)
			((is-division? expr)
				(let ((left (simplify (first expr))) (op '/) (right (simplify (third expr))))
					(cond
						((equal? 0 left) 0)
						(else (list left op right))
					)
				)
			)
			((is-exponent? expr)
				(let ((left (simplify (first expr))) (op '**) (right (simplify (third expr))))
					(cond
						((or (equal? 0 right) (equal? 1 left)) 1)
						((equal? 1 right) left)
						(else (list left op right))
					)
				)
			)
			((is-increment? expr) (+ (simplify (second expr)) 1))
			((is-decrement? expr) (- (simplify (second expr)) 1))
			(else (error "simplify: unidentified expression" expr))
		)
	)
)