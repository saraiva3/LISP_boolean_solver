
(setq p3 '(or 1 a))
(setq p10 '(and x y))
(setq p7 '(or y nil))
(setq p1 '(and x (or x (and y (not z)))))
(setq p4 '(or (and x z) (and y x)))
(setq p15 '(or (and x z) y))
(setq p5 '( not (not 1)))
(setq p9 ' (not (and y x)) )
(setq ande 'and)
(setq var '() )
(setq var2 '())
(setq x '(x))
(setq answer '( ))

(setq rule '(and 1) )
(setq y '(y))
(setq z '(z))
(setq a '(a))
(setq ore 'or) 
(setq note 'not)
(setq comparable '())

(defun evalexp (bindings l size )
	(setq comparable bindings)
	(evalexp2 bindings l size)
)
(defun evalexp2 (bindings l size)
	(setq answer '())
	
	(cond
		((null l) nil)
		((eq (nth 0 (car l)) (car x)) (setq bindings (subst (nth 1(car l)) 'x bindings)) (pop l) (evalexp2 bindings l (- size 1)))
		((eq (nth 0 (car l)) (car y)) (setq bindings (subst (nth 1(car l)) 'y bindings)) (pop l) (evalexp2 bindings l (- size 1) ))
		((eq (nth 0 (car l)) (car a)) (setq bindings (subst (nth 1(car l)) 'a bindings)) (pop l) (evalexp2 bindings l (- size 1) ))
		((eq (nth 0 (car l)) (car z))  (setq bindings (subst (nth 1(car l)) 'z bindings)) (pop l) (evalexp2 bindings l (- size 1)))		
	)

		
	(if (= size 0 )
		(progn		
		(setq bindings(print (simplify nil bindings)))		
		(print "-----")
		(print bindings)	
		(if(equal bindings comparable) 
				bindings
				(if (atom bindings)
			 		(print bindings)
	        (progn (refine  bindings)
				
				(print answer)
		))))))


(defun refine (  bindings)
	(if (atom bindings)
	(progn 
			bindings
			)
			(progn				
			(print (recurse bindings bindings)))))


(defun recurse (tree bindings)
(cond ((null tree) bindings)
((atom tree) bindings)
(T(eq 1 1) (progn
 (recurse (car tree) bindings) (recurse (cdr tree) bindings) 			
 		(if (and (member note tree) (member ore tree))
 			(progn
 				(print tree)
 				(setq answer (nsubst (cons ore (cons (list (cons note(cons var nil) ) ) (list (cons note(cons var2 nil) ) ) ) ) tree bindings) ) 			
 			(if ( listp (nth 0(cdr answer))) 
 			(if(and (member note (nth 0(nth 0(cdr answer)))) (member nil (nth 0(nth 0(cdr answer)))) )
 				(progn 					
 				(setq answer (cons 1 nil))
 			 )))
 			(if ( listp (nth 0(cdr answer))) 
 				(if(and (member note (nth 1(cdr answer))) (member nil (nth 1(cdr answer))) )
 					(progn 					
 					(setq answer (nsubst 1 tree bindings)))
 					(if(and (member note (nth 0(nth 0(cdr answer)))) (member nil (nth 0(nth 0(cdr answer)))) )
 					(progn
 					(setq answer 1)))))))
 		
 		(if(and  (member note tree)( < (length bindings) 5)) 
 			(progn 	
 			(if (member 'y tree) 
 				(setq answer (cons note y ) )
 				(setq answer (cons note (list (nth 1(cdr tree)) ) )))			
 			)
 			(if (and (member note tree) (member ande tree))
 			(progn 
 				(setq answer (nsubst (cons ande (cons (list (cons note(cons var nil) ) ) (list (cons note(cons var2 nil) ) ))  ) tree bindings) )
 				(if(and (member note (nth 1(cdr answer))) (member 1 (nth 1(cdr answer))) )(progn (print " HEEEE")
 					(setq answer (nsubst nil tree bindings)))
 					(if(and  (member note (nth 0(nth 0(cdr answer))))  (member 1 (nth 0(nth 0(cdr answer)))))
 						(progn  							
 						(setq answer (nsubst nil tree bindings))
 						(SETQ tree nil)
 						(PRINT TREE)
 						)
 					(if(and (member note (nth 0(nth 0(cdr answer)))) (member nil (nth 0(nth 0(cdr answer)))) )
 					(progn  						
 					(setq answer tree)))))))) 		

 			
 		(if (member ande tree)
 			(if (member 1 tree)
 					(progn  	
 						
 						(if (equal (nth 0(cdr tree)) 1)
 						(progn  					
 							(setq answer (nsubst (nth 1(cdr tree)) tree bindings)) 									
 						))
 						
 						(if(equal (nth 1(cdr tree)) 1)
 							(progn
 								(setq answer  (nsubst (nth 0(cdr tree)) tree bindings)) 					
 							)))))

 		(if (and ( >(count note tree) 1 )(> (count nil tree) 0) )
 			1
 			(progn
 		(if (member ore tree)
 			(if (member 'nil tree)
 						(progn
 					(if (equal (nth 0(cdr tree)) 'nil)
 						(progn  							
 							(setq answer (nsubst (nth 1(cdr tree)) tree bindings)) 							
 							)

					(if(equal (nth 1(cdr tree)) 'nil)
 							(PROGN
 								(setq answer  (nsubst (nth 0(cdr tree)) tree bindings))
 								
 							)))))))))))
)






(defun simplify (auxBind bindings)

 (cond 
 ((or (atom bindings) (equal auxBind bindings)) bindings)
 (T (simplify bindings (solveAll bindings) ))))

(defun solveAll (bindings)
 (cond 
	 ((equal (car bindings) ande)(solveANd(cdr bindings))) 
	 ((equal (car bindings) ore)(solveOR(cdr bindings)))
	 ((equal (car bindings) note)(solveNot (cdr bindings)))
 )


 )
(defun solveANd (bindings)	
		
(if (find note bindings) (setq answer bindings)(setq answer  (remove 'T (remove-duplicates bindings)))  )
(print (car answer))
 (cond 
 ((and (equal (count note bindings) 2) (equal (count 'nil bindings) 2) )(setq answer 1))
 ((and (equal (count note bindings) 2) (member 'nil bindings) )(setq answer(cons 'and (cons (car answer)( list (nth 0(cdr bindings)) 1 ) ) )))
 ((null answer) 'nil)
 ((member 'nil answer) 'nil)
 ((null (cdr answer))(car answer)) 
	(T (cons 'and (simplify-operand answer)))
 ))
 


(defun solveOR (bindings)

 (if (find note bindings)(setq answer bindings) (setq answer  (remove 'nil (remove-duplicates bindings))) )
 
 (cond 
 ((and (equal (count note bindings) 2) (equal (count 1 bindings) 2) )(setq answer nil))
((and (equal (count note bindings) 1) (equal (count 1 bindings) 1) )(setq answer '(not y)))
 ((and (equal (count note bindings) 2) (member 1 bindings) )(setq answer(cons 'or (cons (car answer)( list (nth 0(cdr bindings)) nil ) ) )))
 ((null answer) 'nil) 
 ((member 1 answer) 1)
 ((null (cdr answer)) (car answer)) 
 (T (cons 'or (simplify-operand answer)))))

(defun solveNot (bindings)

(if(listp (car bindings) )(setq var (nth 1( cdr( car bindings)))))
(if(listp (car bindings) )(setq var2 (nth 0( cdr( car bindings)))))

 (cond 
 ((equal  bindings '(nil)) 1)
 ((equal bindings '(1))  nil) 
  ((equal bindings 1)  nil)
 ((and (listp (car bindings)) (equal (car(car bindings)) ande))(cons ore (cons note (cons var (cons note (cons var2 nil))) )) )
 ((and (listp (car bindings)) (equal (car(car bindings)) ore))(cons ande (cons note (cons var (cons note (cons var2 nil))) )) ) 	
  ((and (listp (car bindings)) (equal (car (car bindings)) 'not)) (car (rest (car
bindings))))
 (T (cons 'not (simplify-operand bindings)))
 )

 )
(defun simplify-operand (values)

 (cond 
 ((null values) NIL) 
 ((atom (car values)) (cons (car values) (simplify-operand (cdr values))))
 (T (cons (simplify nil (car values))(simplify-operand (cdr values)))

 	)
 )
  )

