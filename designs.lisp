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

;; 
;;; Testing 
;;

(define-design desg1 (a b c d e)
  (satisfying-relations
   (+ a e  b 5)
   (- b c)
   (+ d 5 (sinh e))
   (- b  d 2)))

(defun iafb3 ()
  (iterate-and-find-bounds #'desg1
						   '(:a (1 3 8)
							 :b (2 3)
							 :c 4)))


(defparameter *system* nil)
(defmacro with-new-system (var &body body)
  (assert (typep var 'symbol))
  `(let* ((*system* (make-instance 'system))
		  ,@(when var `((,var *system*))))
	 ,@body
	 (solve-system *system*)))

(defmacro finally (&body body)
  "Run the body on completion of the system-solution"
  (alexandria:with-gensyms (system choice-point)
	`(let ((,choice-point *choice-point*))
	   (add-solution-finalizer (alexandria:named-lambda finally  (,system)
								 (let ((*system* ,system)
									   (*choice-point* ,choice-point))
								   ,@body))
							   *system*))))
	

(defun tt()
  (with-new-system ()
	(with-parameters (a b)
	  (satisfying-relations (+ a b 5)
							(+ (* 2 a) (* 3 b) 9))
	  (finally 
		(if (> (value a) -10)
			(with-parameters (c d)
			  (satisfying-relations (= c (+ 12 a))
									(+ d c a (/ 9 a)))
			  (solve-system)))))))

(defun tt()
  (with-new-system s
	(with-parameters (a b)
	  (let ((relations (satisfying-relations (+ a b 5)
											 (+ (* 2 a) (* 3 b) 9))))
		(finally
		  (if (> (value a) -10)
			  (progn
				(remove-relations relations s)
				(with-parameters (c d)
				  (satisfying-relations (= c (+ 12 a))
										(+ d c a (/ 9 a)))
				  (solve-system s)))))))))

(defun tt2 ()
  (with-new-system s
	(with-parameters (a b)
	  (list a b  s))))


(defparameter *choice-point* nil)
(declaim (special *choice-point*))
(defclass choice-point ()
  ((p :type parameter :initarg :p)
   (choices :type list :initarg :choices)
   (exhausted-choices :type list :initform nil)
   (body :type function :initarg :body )
   (relations :type list :initarg :relations)
   (parameters :type list :initarg :parameters)
   (new-relations :type list :initform nil)
   (solution-finalizer :type function :initarg :old-solution-finalizer)
   (parent-choice-point :type choice-point :initarg :parent-choice-point :initform nil)))

;;
;;; TRY IMPLEMENTING CHOICE POINTS WITH CONDITIONS AND RESTARTS
;;
(define-condition choice-exhausted (error) ())

(defun failed (&optional (choice-point *choice-point*) (system *system*))
  (cond ((and choice-point system)
		 (with-slots (p choices exhausted-choices relations parameters parent-choice-point
					  new-relations) choice-point
		   (cond ((null choices)
				  (if parent-choice-point
					  (failed parent-choice-point)
					  (error 'choice-exhausted)))
				 (t
				  (let ((v (pop choices)))
					(push v exhausted-choices)
					(setf (value p) v
						  (slot-value p 'solvable-state) :known))
				  
				  (remove-relations new-relations system)				  
				  (setf (relations system) relations
						(parameters system) parameters)

;;				  (loop for p in (slot-value choice-point 'new-parameters) do
;; 					(remove-parameter p system))
;; ;;				  (setf (slot-value system 'solution-finalizer) old-solution-finalizer)
;; ;;				  (print old-solution-finalizer)
				  (format t "trying ~a~%" (value p))


				  
				  (funcall (slot-value choice-point 'body))))))
		((not choice-point)
		 (error "can't handle fail without choice-point"))
		((not system)
		 (error "no system defined"))))
				  
(defmacro with-choice-point ((param choice-type) &rest body)
  (alexandria:with-gensyms (choice-point)
	`(let ((,choice-point 
			 (make-instance 'choice-point
							:p ,param
							:choices ,(cond ((and (listp choice-type)
												  (eql (first choice-type) :member))
											 `(list ,@(rest choice-type)))
											(t (error "Unknow choice type ~a, (:member ...) are only allowed"
													  choice-type)))
							:parameters (parameters *system*)
							:relations (relations *system*)
							:parent-choice-point *choice-point*)))
	   
	   (let ((*choice-point* ,choice-point))
		 (setf (slot-value ,choice-point 'body)
			   (alexandria:named-lambda with-choice-point ()
				 ,@body))
				 ;; (setf (slot-value ,choice-point 'old-solution-finalizer)
				 ;; 	   (slot-value *system* 'solution-finalizer))))
		 (failed)))))


;; (define-condition choice-failed (error) ())
(define-condition choice-exhausted (error) ())

;; (defmacro with-choice-point ((param choice-type) &rest body)
;;   `(let ((choices ,(rest choice-type)))
;; 	 (handler-bind ((choice-failed
;; 					 #'(lambda (err)
;; 						 (declare (ignore err))
;; 						 (with-slots (p choices exhausted-choices) choice-point
;; 						   (cond ((null choices)
;; 								  (error 'choice-exhausted))
;; 								 (t
;; 								  (let ((v (pop choices)))
;; 									(push v exhausted-choices)
;; 									(setf (value p v)))
;; 								  (remove-relations new-relations *system*)


	  

(defun tt3()
  (with-new-system ()
	  (with-parameters (sh q)
		(with-choice-point (sh (:member :sq :rect :circle))
		  (if (eql (value sh) :sq)
			  (satisfying-relations (- q 10)))
		  (if (eql (value sh) :rect)
			  (satisfying-relations (- q 5)))
		  (if (eql (value sh) :circle)
			  (satisfying-relations (- q 15)))
		  (inspect *system*)
		  (print "cc")
		  (finally
			(print "checking if q<12")
			(if (< (value q) 12)
				(failed)))
		  (solve-system))

		(print "hello"))))
