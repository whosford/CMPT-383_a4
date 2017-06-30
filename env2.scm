;This implementation of the environment ADT uses closures and treats the environment as a recursive function.
;If the make-empty-env function is called it will always return an error as the variable has not been bound.
;The extend-env function returns the value of the specified variable. If the variable is not found in the current environment then
;extend-env is called again to check the next environment, finally if the variable is not founc in any of the environments then the
;make-empty-env function is called which returns an error.
;The apply-env function just calls the environment function on the variable that has been supplied.

(define make-empty-env
	(lambda ()
    	(lambda (v) 
      		(error "apply-env: variable not bound")
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