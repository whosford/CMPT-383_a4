;This implementation of the environment ADT uses a list structure.
;The make-empty-env function returns an empty list.
;The apply-env function searches the list for the input variable and returns the value associated with it or null if it is not in the list.
;The extend-env function returns a new list with the new variable and value added. If the variable is already in the list then its value
;is changed to the new value.

(define in-environment
	(lambda (env v)
		(cond
			((null? env) 
				'())
			((equal? v (car (car env))) 
				(car (cdr (car env))))
			(else 
				(in-environment (cdr env) v))
		)
	)
)

(define add-to-env
	(lambda (v val env)
		(cons (list v val) env)
	)
)

(define replace-in-env
	(lambda (v val env)
		(if (equal? v (car (car env))) 
			(cons (list v val) (cdr env))
			(cons (car env) (replace-in-env v val (cdr env)))
		)
	)
)

(define make-empty-env 
	(lambda ()
		'()
	)
)

(define apply-env
	(lambda (env v)
		(if (null? (in-environment env v))
			(error "apply-env: variable not in environment")
			(in-environment env v)
		)
	)
)

(define extend-env
	(lambda (v val env)
		(if (null? (in-environment env v))
			(add-to-env v val env)
			(replace-in-env v val env)
		)
	)
)