(defmacro my-let (varlist &body body)
	   `((lambda ,(mapcar #'car varlist)
	       ,@body)
	     ,@(mapcar #'cadr varlist)))

(defun compose-help (lst x)
	   (if (= 1 (length lst))
	       `(apply #',(car lst) ,x)
	       `(,(car lst) ,(compose-help (cdr lst) x))))

(defmacro compose (&rest lst)
	   (let ((arg (gensym)))
	     `(lambda (&rest ,arg)
		,(compose-help lst arg))))

(set-macro-character #\} (get-macro-character #\) ))

(set-dispatch-macro-character #\# #\{
				       (lambda (stream ch1 ch2)
					 (declare (ignore ch1 ch2))
					 (let ((lst (read-delimited-list #\} stream t)))
					   `(compose ,@lst))))

(let ((rpar (get-macro-character #\))))
	   (defun ddfn (left right fn)
	     (set-macro-character right rpar)
	     (set-dispatch-macro-character #\# left
		 (lambda (stream ch1 ch2)
		   (declare (ignore ch1 ch2))
		   (apply fn
			  (read-delimited-list right stream t))))))

(defmacro defdelim (left right parms &body body)
	   `(ddfn ,left ,right #'(lambda ,parms ,@body)))


(defun fltn (lst)
	   (labels ((rec (x acc)
		      (if (null x)
			  acc
		      (let ((y (car x)))
			(if (atom y)
			    (rec (cdr x) (append acc (list y) ) )
			    (rec (cdr x) (rec y acc)))))))
	     (rec lst nil)))