(define make-empty-env
	(lambda ()
    	(lambda (v) 
      		'()
      	)
    )
)

(define extend-env
	(lambda (v val env)
		(lambda (x)
			(if (equal? x v)
				val
				(apply-env env x)
			)
		)
	)
)

(define apply-env
	(lambda (env v) 
		(env v)
	)
)

(define myeval 
	(lambda (expr env)
		(cond
			((number? expr)
				expr)
			((symbol? expr)
				(if (null? (apply-env env expr))
					(error "myeval: unknown variable" expr)
					(apply-env env expr)))
			(else
				(if (or (equal? (first expr) 'inc) (equal? (first expr) 'dec))
					(let ((op (first expr)) (right (myeval (second expr) env)))
						(cond 
							((equal? op 'inc) (+ right 1))
							((equal? op 'dec) (- right 1))
							(else (error "myeval: unknown operator" op))
						)
					)
					(let ((left (myeval (first expr) env)) (op (second expr)) (right (myeval (third expr) env)))
						(cond
							((equal? op '+) (+ left right))
							((equal? op '-) (- left right))
							((equal? op '*) (* left right))
							((equal? op '/) (if (= right 0) (error "myeval: cannot divide by zero") (/ left right)))
							((equal? op '**) (expt left right))
							(else (error "myeval: unknown operator" op))
						)
					)
				)			
			)
		)
	)
)