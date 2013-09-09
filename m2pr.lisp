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