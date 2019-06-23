(in-package :system-solver)

(defun derivative (f x j)
  "Compute partial derivative of f with respect to x_j at x"
  (let ((f1 (funcall f x))
	f2)
    (incf (grid:aref x j) 0.0001d0)
    (setf f2 (funcall f x))
    (decf (grid:aref x j) 0.0001d0)
    (/ (- f2 f1) 0.0001d0)))

(defparameter *last-system* nil)
(defun solve-system4 (functions initial-guess &optional (iters 100))
  (setf *last-system* (list :functions functions :guess initial-guess) )
  (let* ((number-of-functions (length functions))
	 (number-of-parameters (length initial-guess))
	 residue
	 (init (grid:make-foreign-array
		'double-float
		:initial-contents initial-guess)))
    (flet ((residual (x f)
	     ;; (print x)
	     (setf residue nil)
	       (dotimes (i number-of-functions)
		 (setf (grid:aref f i)
		       (realpart (+ (funcall (nth i functions) x))))
		 (push (grid:aref f i) residue))
	       ;; (print (reverse residue)))
	       )
	       
	   (residual-derivative (x jacobian)
	     (dotimes (i number-of-functions)
	       (dotimes (j number-of-parameters)
		 (setf (grid:aref jacobian i j)
		       (realpart (derivative (nth i functions) x j)))))
	     ;; (print "jacobian")
	     ;; (print jacobian)
	     ))
      (let ((fit (gsll:make-nonlinear-fdffit
		  gsll:+levenberg-marquardt+
		  (list number-of-functions number-of-parameters)
		  (list #'residual #'residual-derivative)
		  init nil)))	 
	(loop for iter from 0 below iters
	   until (and (plusp iter)
		      (gsll:fit-test-delta fit 1.0d-4 1.0d-4)
		      (= (count t residue :key (lambda (x) (< (abs x) 1.0d-4)))
			 (length residue)))
	   do
	     ;; (print (gsll:solution fit))
	     (gsll:iterate fit)
	   finally
	     (format t "~%Took ~d iterations" iter)
	     (return (gsll:solution fit)))))))
	


;;;; TEST
(defun test-solve1 ()
  (flet ((xi (x i) (grid:aref x i)))
    (solve-system4 (list (lambda (x)
			  (+ (* (expt (xi x 0) 2) (xi x 1)) -2))
			(lambda (x)
			  (+ (xi x 0) (- (xi x 1)) -4))
			(lambda (x)
			  (+ (xi x 0) (xi x 1) -5.20d0)))
		  (list 0.0d0 1.0d0))))

(defun create-evaluator (relation parameters)
  (let* ((params (slot-value relation 'parameters))
	 (pos (loop for param in params
		 collect (position param parameters))))
    ;; (print (list params pos parameters))
    (lambda (x)
      (loop for i in pos
	 for p in params do
	   (when i (setf (value p) (grid:aref x i))))
      (eval-relation relation))))
	 

(defun solve-system5 (relations parameters)
  (print-parameters parameters)
  ;; (inspect relations)
  (let ((soln
	 (solve-system4 (loop for r in relations collect (create-evaluator r parameters))
		 (loop for p in parameters collect
		      (if (slot-boundp p 'value)
			  (value p)
			  (setf (value p) (random 100)))))))
    (loop for p in parameters
       for i from 0 do
	 (setf (value p) (grid:aref soln i)))))
  
					
(defun residue (fs xs)
  (setf xs (grid:make-foreign-array 'double-float :initial-contents xs))
  (loop for f in fs
     collect (realpart (funcall f xs))))

(defun solve-system-saved (sys)
  (solve-system4 (getf sys :functions)
		 (getf sys :guess)))
