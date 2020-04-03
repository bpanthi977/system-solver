(in-package #:system-solver)

;;
;;; Base
;;

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
			 collect (cons p-symbol (safe-value p))))))

(export 'define-design)
(defmacro define-design (name parameters &rest body)
  `(setf (symbol-function ',name)
		 (make-design ,parameters ,@body)))

;;
;;; Bounds finding
;;

(defun collect-parameters-with-bounds0 (parameters iterations)
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

(defun collect-parameters-with-bounds (iterations)
  (when iterations
	(collect-parameters-with-bounds0 (mapcar #'car (first iterations))
									 iterations)))

(defun iafb (design choosen pinits results)
  (unless pinits
	(return-from iafb (push (apply design choosen) results)))
  
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
  (collect-parameters-with-bounds (iafb design nil parameters-initialization nil)))

;; 
;;; Testing 
;;

;; (define-design desg1 (a b c d e)
;;   (satisfying-relations
;;    (+ a e  b 5)
;;    (- b c)
;;    (+ d 5 (sinh e))
;;    (- b  d 2)))

;; (defun iafb2 ()
;;   (let ((a '(1 3 8))
;; 		(b '(2 3))
;; 		(c 4)
;; 		iterations)
;; 	(loop for a in a do 
;; 	  (loop for b in b do 
;; 		(push (desg1 :a a :b b :c c)
;; 			  iterations)))
;; 	(collect-parameters-with-bounds0 '(a b c d e) iterations)))

;; (defun iafb3 ()
;;   (iterate-and-find-bounds #'desg1
;; 						   '(:a (1 3 8)
;; 							 :b (2 3)
;; 							 :c 4)))










