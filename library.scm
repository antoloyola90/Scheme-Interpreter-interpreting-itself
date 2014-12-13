(define ( not n )
		( if n #f #t ))	

(define (and n1 n2)
  (if n1 n2 #f))	
  
(define (or n1 n2)
  (if n1 #t n2))  
  
(define (equal? n1 n2)
	(if (pair? n1) (if (eq? (car n1) (car n2)) #t #f) (if (eq? n1 n2) #t  #f))
)	  

(define ( cddr n) ( cdr ( cdr n )))
		  
(define ( caar n ) ( car ( car n) ))  

(define ( cadr n ) (car (cdr n)))
		
(define ( cdar n ) ( cdr ( car n )))
		
(define	( cadar n ) ( car ( cdr ( car n ))))
		
(define ( caddr n ) ( car ( cdr ( cdr n ))))
		
(define ( cadddr n ) ( car ( cdr ( cdr ( cdr n )))))

(define (assoc var n)
	(cond ((null? n) #f) ((eq? var (car (car n))) (car n))
	(else (assoc var (cdr n)))))
	
(define (map f n)
   (cond ((null? n) '())
   (else (cons (f (car n)) (map f (cdr n))))))
   
(define (append n1 n2)
    (cond ((null? n1) n2)
    	(else (cons (car n1) (append (cdr n1) n2))))
	)
   
   