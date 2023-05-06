Church-compiler

Author : Rainfield Mak


Usage : A primitive church compiler that run in Racket 

 Input language:

 e ::= (letrec ([x (lambda (x ...) e)]) e)    
     | (let ([x e] ...) e)  
     | (let* ([x e] ...) e)
     | (lambda (x ...) e)
     | (e e ...)    
     | x  
     | (and e ...) | (or e ...)
     | (if e e e)
     | (prim e) | (prim e e)
     | datum
 datum ::= nat | (quote ()) | #t | #f 
 nat ::= 0 | 1 | 2 | ... 
 x is a symbol
 prim is a primitive operation in list prims
 The following are *extra credit*: -, =, sub1  
(define prims '(+ * - = add1 sub1 cons car cdr null? not zero?))

 This input language has semantics identical to Scheme / Racket, except:
   + You will not be provided code that yields any kind of error in Racket
   + You do not need to treat non-boolean values as #t at if, and, or forms
   + primitive operations are either strictly unary (add1 sub1 null? zero? not car cdr), 
                                           or binary (+ - * = cons)
   + There will be no variadic functions or applications---but any fixed arity is allowed

 Output language:

 e ::= (lambda (x) e)
     | (e e)
     | x

 also as interpreted by Racket
