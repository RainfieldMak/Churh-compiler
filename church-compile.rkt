#lang racket

;; Assignment 4: A church-compiler for Scheme, to Lambda-calculus

(provide church-compile
         ; provided conversions:
         church->nat
         church->bool
         church->listof
	churchify)





;; Using the following decoding functions:

; A church-encoded nat is a function taking an f, and x, returning (f^n x)
(define (church->nat c-nat)
  ((c-nat add1) 0))

; A church-encoded bool is a function taking a true-thunk and false-thunk,
;   returning (true-thunk) when true, and (false-thunk) when false
(define (church->bool c-bool)
  ((c-bool (lambda (_) #t)) (lambda (_) #f)))

; A church-encoded cons-cell is a function taking a when-cons callback, and a when-null callback (thunk),
;   returning when-cons applied on the car and cdr elements
; A church-encoded cons-cell is a function taking a when-cons callback, and a when-null callback (thunk),
;   returning the when-null thunk, applied on a dummy value (arbitrary value that will be thrown away)
(define ((church->listof T) c-lst)
  ; when it's a pair, convert the element with T, and the tail with (church->listof T)
  ((c-lst (lambda (a) (lambda (b) (cons (T a) ((church->listof T) b)))))
   ; when it's null, return Racket's null
   (lambda (_) '())))




; churchify recursively walks the AST and converts each expression in the input language (defined above)
;   to an equivalent (when converted back via each church->XYZ) expression in the output language (defined above)
(define (churchify e)
	


	(match e
         
		;Boolean
		[#t (churchify '(lambda (x y)  (x x) ))]

		[#f (churchify '(lambda (x y)  (y y) ))]

		;Let
		[`(let ([,x ,rhs])   ,body )
		(churchify `((lambda (,x ) ,body) ,rhs))]

		[`(let ([,x ,xrhs]  ...) ,body)
			(churchify `((lambda (,@x) ,body) ,@xrhs))
		]

		;if 
		[`(if ,g ,t ,f) (churchify  
					`(
						( ,g  
							(lambda (_) ,t)
						) 
						(lambda (_) ,f)

					  ) 
				    ) 

		]

		
		;and

		[`(and ,p ,q) (churchify `(if ,p ,q #f)) ]

		[`(and ,p ,q ,q+ ...)    (churchify `(if ,p (and ,q ,@q+)  ,#f )       )   ]
		
		;[`(and ,p ,q) (churchify `(lambda (,p ,q) (,p ,q ,#f)) )]

		;[`(and ,p ,q ,x ...) (churchify `(and ,p  (,q ,@x) )  )]



		;or 
		[`(or ,p ,q)  (churchify `(if ,p ,#t ,q)) ]
		
		[`(or ,p ,q ,q+ ...)  (churchify `(if ,p ,#t (or ,q ,@q+)  )) ]

		
		;not
		[`(not ,p) (churchify `(if ,p ,#f ,#t))]



		;list
		;null
		['()
			(churchify (lambda (when-cons)(lambda  (when-null) (when-null (lambda (x) x))))
					
			)

		]
		
		['cons  (churchify `(lambda (a b) (lambda (when-cons when-null) (when-cons a b))))


		]

		['car (churchify `(lambda (p) (
								(p 
									(lambda (a)(lambda  (b) a))
							  	)	 
								(lambda () ((lambda (u) ( u u)) (lambda (u) ( u u)))  )
							)
					)

			)
							    
		]

		['cdr (churchify `(lambda (p) (
								(p 
									(lambda (a)(lambda  (b) b))
							  	)	 
								(lambda () ((lambda (u) ( u u)) (lambda (u) ( u u)))  )
							)
					)

			)
							    
		]

		['null? (churchify `(lambda (cons-cell)
							(
								(cons-cell (lambda (a b) , #f))

							 ( lambda () ,#t)
							)


					   )



			)]
		


		;let*
		[`(let* ([,x ,rhs])   ,body )
		(churchify `((lambda (,x ) ,body) ,rhs))]

		[`(let* ([,x ,xrhs]  ,y ...) ,body)
			`((lambda (,x) ,(churchify `(let* (,@y) ,body )) ) ,(churchify xrhs))
		]


		
		;curry lambda
		[`(lambda () ,body)
		`(lambda (_) ,(churchify body))]
		
		[`(lambda (,x) ,body)
		`(lambda (,x) ,(churchify body))]

		[`(lambda (,x ,ys ...) ,body)
		`(lambda (,x) ,(churchify `(lambda ,ys ,body)))]

		;curry application
		[`(,ef)  
		  `(,(churchify ef)  (lambda (x) x))      ] 

		[`(,ef ,earg) `(,(churchify ef) ,(churchify earg)  )]

		[`(,ef ,earg0 ,eargs ...) (churchify `((,ef ,earg0) ,@eargs) )]



		;literals
		[(? integer? n)
			(define (wrap n)
				(if (zero? n)
					'x
					`(f ,(wrap (- n 1)))

				)
			)
			(churchify `(lambda (f x) ,(wrap n)))

		]
		
		[_ e]
	)
)

; Takes a whole program in the input language, and converts it into an equivalent program in lambda-calc
(define (church-compile program)
  ; Define primitive operations and needed helpers using a top-level let form?

  (define church+ `(lambda (m n) (lambda (f x) (n f (m f x)) ) ))
  (define church* `(lambda (n m)  (lambda (f x) ((n (m f)) x)))     )
  (define church-add-1 `(lambda(n) (lambda (f) (lambda (x) (f((n f) x))))))

  (define church-cons  `(lambda (a b) (lambda (when-cons when-null) (when-cons a b))))
  (define church-car `(lambda (p) (p (lambda (a b) a) (lambda () omega))))
  (define church-cdr `(lambda (p) (p (lambda (a b) b) (lambda () omega))))


  (churchify
   `(let (
         
	  [+ ,church+]	  
	  [* ,church*]
	  [add1 ,church-add-1]	
	  [cons ,church-cons ]
	  [car, church-car]
	  [cdr, church-car] 
	  		
          
          )
      ,program)))
