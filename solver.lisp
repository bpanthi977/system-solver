(in-package :system-solver)
(setf *read-default-float-format* 'double-float)

;; SINGLE VARIABLE SCALAR FUNCTION SOLVERS 
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


;; MULTI VARIABLE VECTOR FUNCTION SOLVERS 
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
