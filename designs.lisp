(in-package #:system-solver)

;;
;;; Base
;;
(defclass design ()
  ((parameter-names :initarg :parameter-names :initform nil)
   (parameter-types :initarg :parameter-types :initform 'number)))

(defun safe-value (param &optional default)
  (if (slot-boundp param 'value)
	  (slot-value param 'value)
	  default))

(defmacro make-design (parameters &rest body)
  `(lambda (&key ,@parameters)
	 (with-parameters (,@(mapcar #'(lambda (p) (list p p)) parameters))
	   ,@body 
	   (solve-for (list ,@parameters))
	   (loop for p in (list ,@parameters)
		  for p-symbol in ',parameters
		  collect p-symbol
		  collect (safe-value p)))))

(export 'define-design)
(defmacro define-design (name parameters &rest body)
  "Returns a function that solves for the `parameters' by evaluating the `body' doing a solve-for, and then 
return a plist with values"
  `(setf (symbol-function ',name)
		 (make-design ,parameters ,@body)
		 (getf (symbol-plist ',name) :design-info) (make-instance 'design :parameter-names ',parameters)))

;;
;;; Bounds finding
;;

(defun collect-parameters-with-bounds0 (parameters iterations)
  (loop for p in parameters
		for upper = nil
		for lower = nil do
		  (loop for i in iterations
				for v = (getf i p) do
				  (setf upper (if upper
								  (if v (max upper v) upper)
								  (if v v nil))
						lower (if lower
								  (if v (min lower v) lower)
								  (if v v nil))))
	 collect p
	 collect (if upper (if (= upper lower) upper (list lower upper)) nil)))

(defun collect-parameters-with-bounds (iterations)
  (when iterations
	(collect-parameters-with-bounds0 (loop for p in (first iterations) by #'cddr collect p)
									 iterations)))

(defun iterate-and-find-bounds0 (design choosen pinits results)
  (unless pinits
	(return-from iterate-and-find-bounds0 (push (apply design choosen) results)))
  
  (destructuring-bind (p v . rest)  pinits 
	(loop for val in (alexandria:ensure-list v) do
	  (setf results
			(iafb design
				  `(,p ,val ,@choosen)
				  rest
				  results)))
	results))

(export 'iterate-and-find-bounds)
(defun iterate-and-find-bounds (design parameters-initialization)
  ;; improviastaion : sort the params by least variation first
  (collect-parameters-with-bounds (iterate-and-find-bounds0 design nil parameters-initialization nil)))

;;;
;; EXAMPLES
;;;


(define-design ex-desg1 (a b c d e)
  (satisfying-relations
   (+ a e  b 5)
   (- b c)
   (+ d 5 (sinh e))
   (- b  d 2)))

(defun ex-iafb1 ()
  (iterate-and-find-bounds #'desg1
						   '(:a (1 3 8)
							 :b (2 3)
							 :c 4)))


