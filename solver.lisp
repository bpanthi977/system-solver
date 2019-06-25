(in-package :system-solver)
(setf *read-default-float-format* 'double-float)

(defun newton-solve (g g-prime &optional (x0 1) (tolerance 1d-6) (max-iterations 50))
  "Solve for root of g(x) = 0 within tolerance;
 given derivative of g (g-prime) and initial guess x0"
  (let ((g0 (funcall g x0))
	g-prime0)	
    (loop for i from 0 to max-iterations do
	 (when (< (abs g0) tolerance)
	   (return (values x0 tolerance i)))

	 (setf g-prime0 (funcall g-prime x0))
	 (setf x0 (- x0 (/ g0 g-prime0)))
	 (setf g0 (funcall g x0))
       finally (return (values x0 g0 max-iterations)))))

(defun newton-solver-general (g &optional (x0 1) (tolerance 0.00001) (max-iterations 50) (step  0.01))
  (let ((g-derivative #'(lambda (x)
			  (/ (- (funcall g (+ x step)) (funcall g x))
			     step))))
    (newton-solve g g-derivative x0 tolerance max-iterations)))

(defun derivative (f x j)
  "Compute partial derivative of f with respect to x_j at x"
  (let ((f1 (funcall f x))
	f2)
    (incf (grid:aref x j) 0.0001d0)
    (setf f2 (funcall f x))
    (decf (grid:aref x j) 0.0001d0)
    (/ (- f2 f1) 0.0001d0)))

(defun levenberg-solve (functions initial-guess &optional (iters 100))
  "Solve a system of functions using Levenberg-Marquardt Algorithm"
  (let* ((number-of-functions (length functions))
	 (number-of-parameters (length initial-guess))
	 residue
	 (init (grid:make-foreign-array
		'double-float
		:initial-contents initial-guess)))
    (flet ((residual (x f)
	     (setf residue nil)
	       (dotimes (i number-of-functions)
		 (setf (grid:aref f i)
		       (coerce (realpart (+ (funcall (nth i functions) x))) 'double-float))
		 (push (grid:aref f i) residue)))	       
	   (residual-derivative (x jacobian)
	     (dotimes (i number-of-functions)
	       (dotimes (j number-of-parameters)
		 (setf (grid:aref jacobian i j)
		       (coerce (realpart (derivative (nth i functions) x j)) 'double-float))))))
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
	     (gsll:iterate fit)
	   finally
	     (format t "~%Took ~d iterations" iter)
	     (return (gsll:solution fit)))))))

;; ;;;; Self made solver

;; (defun eval-functions (functions)
;;   (map 'vector #'eval-relation functions))

;; (defun norm (vector)
;;   (sqrt (iterate:iter (iterate:for i in-vector vector)
;; 		      (iterate:summing (expt i 2)))))

;; (defun norm2 (vector)
;;   (loop for i across vector
;;      summing i))

;; (defun solve-system0 (functions parameters initial &optional (increment 0.1))
;;   (loop
;;      for x in parameters
;;      for x0 = (value x)
;;      for x1 = (incf (value x) increment) 
;;      for new = (eval-functions functions)
;;      for dv = (map 'vector (lambda (a b) (* (signum a) (- b a)))
;; 		   initial new)
;;      for norm2-dv = (norm2 dv)
;;      for new-x = (if (= 0 norm2-dv) x1 (- x0  (* (norm initial) increment (/ norm2-dv))))
;;      with new-values = nil do
;;      ;; (print (list x0 x1 new dv norm2-dv new-x))
;;        (setf (value x) x0
;; 	     initial (eval-functions functions))
;;      ;; (print new-values)
;;        (push new-x new-values)
;;      finally (progn
;; 	       (loop for p in parameters
;; 		  for v in (reverse new-values) do
;; 		    (setf (value p) v))
;; 	       (return initial))))

;; (defun solve-system (functions parameters &optional (max-iterations 10) (tolerance 0.1))
;;   (loop for p in parameters do
;;        (unless (slot-boundp p 'value)
;; 	 (setf (value p) 1)))
;;   (loop for x from 1 to max-iterations
;;      for v_n = (eval-functions functions)
;;      for v_n+1 = (solve-system0 functions parameters v_n) do
;;      ;; (print-parameters parameters)
;;        ;; (print v_n+1)
;;        ;; (print (norm v_n+1))
;;        (if (< (norm v_n+1) tolerance)
;; 	   (return t))
;;      finally
;;        (progn
;; 	 (print "Wasn't within tolerance"))))


;; ;; SIMPLE SUBSTITUTION Solver

;; (defun execute-solution-strategy0 (relations parameters) 
;;   (loop
;;      for r in relations
;;      for p in parameters do
;;        (solve-relation p r)))

;; (defun parameter-values (parameters)
;;   (loop for p in parameters collect (value p)))

;; (defun execute-solution-strategy (unknown relations parameters
;; 				  &optional (initial-guess 1) (tolerance 0.0001))
;;   (unless relations
;;     (print "No strategy to execute")
;;     (return-from execute-solution-strategy (value unknown)))
;;   (loop for p in parameters do       
;;        (unless (slot-boundp p 'value)
;; 	 (setf (slot-value p 'value) 0)))
;;   (setf (value unknown) initial-guess)
;;   (loop for count from 0 to 100
;;      for previous-values = (parameter-values parameters)
;;      for unknown-prev-value = (value unknown)
;;      with params-stable = nil do 
;;        (execute-solution-strategy0 relations parameters)
;;      ;; (print unknown-prev-value)
;;        (when (< (abs (- unknown-prev-value (value unknown))) tolerance)
;; 	 (loop for p in parameters
;; 	    for vp in previous-values do 
;; 	      (unless (< (abs (- vp (value p))) tolerance)
;; 		(format t "Not within tolerance ~a" (slot-value p 'name))
;; 		(return))
;; 	    finally (setf params-stable t))
;; 	 (when params-stable
;; 	   (return (value unknown)))
;; 	 (setf params-stable nil))
;;      finally (progn (print "Iterated 10 times, not within tolerance")
;; 		    (value unknown))))

;; (defun solve-by-substitution (unknown/s)
;;   (screamer:one-value
;;    (let (relations parameters unknown)
;;      (loop for u in (alexandria:ensure-list unknown/s) do
;; 	  (setf unknown u)
;; 	  (multiple-value-bind (r p) (solve-parameter unknown
;; 						      :traversed-parameters parameters
;; 						      :traversed-relations relations)
;; 	    (setf relations (append r relations))
;; 	    (setf parameters (append p parameters))))
;;      (print-parameters parameters)
;;      (print relations)
;;      (print "Executing strategy")
;;      (execute-solution-strategy unknown relations parameters)
;;      (print-parameters parameters)
;;      (print (value unknown)))))
