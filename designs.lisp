(in-package #:system-solver)

(defmacro define-design (name parameters &rest body)
  `(progn
	 (setf (getf (symbol-plist ',name) 'design-parameters) ',parameters)
	 (setf (getf (symbol-plist ',name) 'design-body) ',body)))

(defun safe-value (param &optional default)
  (if (slot-boundp param 'value)
	  (slot-value param 'value)
	  default))

(defmacro solve-design (name &rest parameters-initialization)
  (flet ((append-missing-parameters (given required)
		   (loop for r in required
				 collect (or (find r given :key #'(lambda (spec)
													(if (listp spec)
														(first spec)
														spec)))
							 r))))
;;	(alexandria:with-gensyms (system)
	  (let ((parameters (getf (symbol-plist name) 'design-parameters)))
		`(with-parameters ,(append-missing-parameters parameters-initialization parameters)
		   ,@(getf (symbol-plist name) 'design-body)
		   ;; (let ((,system (solve-for (list ,@parameters))))
		   (solve-for (list ,@parameters))
		   (loop for p in (list ,@parameters)
				 for p-symbol in ',parameters
				 collect (cons p-symbol (safe-value p)))))))

(defun iterate-and-find-bounds (design parameters-initialization)
  (solve-design design parameters-initialization))

;;
;;; Testing 
;;

(define-design desg1 (a b c d e)
  (satisfying-relations
   (+ a b 5)
   (- b c)
   (+ d 5 (sinh e))
   (- b a d 2)))

(defun collect-parameters-with-bounds (parameters iterations)
  (loop for p in parameters
		for upper = nil
		for lower = nil do
		  (loop for i in iterations
				for (_ . v) = (assoc p i) do
				  (setf upper (if upper
								  (if v (max upper v) upper)
								  (if v v nil))
						lower (if lower
								  (if v (min lower v) lower)
								  (if v v nil))))
		collect (cons p (list (if upper (list lower upper) nil)))))

(defun iafb ()
  (let ((a '(1 8))
		(b '(2 3))
		(c 4)
		iterations)
	(loop for a in a do 
	  (loop for b in b do 
		(push (solve-design desg1 (a a) (b b) (c c))
			  iterations)))
	(collect-parameters-with-bounds '(a b c d e) iterations)))










