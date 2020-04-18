(defparameter *choice-point* nil)
(declaim (special *choice-point*))

(defclass choice-point ()
  ((p :type parameter :initarg :p)
   (choices :type list :initarg :choices)
   (exhausted-choices :type list :initform nil)
   (body :type function :initarg :body )
   (old-system :type system :initarg :old-system)
   (parent-choice-point :type choice-point :initarg :parent-choice-point :initform nil))
  (:documentation "Choice point iterates the parameter 'p' through values 'choices'
The body is evaluated whenever the failed function is called, the system is restored to its original values 
and next choice of 'p' is taken.
Always use with-choice-point to establish choice points and call solve-system within the body to compute 
new values of other parameters that may affect the failing conditions"))

;;; TOTHINK:: TRY IMPLEMENTING CHOICE POINTS WITH CONDITIONS AND RESTARTS

(define-condition choice-exhausted (error) ())

(defun failed (&optional (choice-point *choice-point*) (system *system*))
  (cond ((and choice-point system)
		 (with-slots (p choices exhausted-choices old-system parent-choice-point) choice-point
					  
		   (cond ((null choices)
				  (if parent-choice-point
					  (failed parent-choice-point)
					  (error 'choice-exhausted)))
				 (t
				  ;; set new choice of parameter
				  (let ((v (pop choices)))
					(push v exhausted-choices)
					(setf (value p) v
						  (slot-value p 'solvable-state) :known))
				  ;; invalidate newly added relations from parameters
				  (dolist (r (set-difference (relations system) (relations old-system)))
					(invalidate-relation r))
				  ;; establish old system values
				  (copy-system old-system system)
				  
				  (funcall (slot-value choice-point 'body))))))
		((not choice-point)
		 (error "can't handle fail without choice-point"))
		((not system)
		 (error "no system defined"))))


;; INTERFACE
(defmacro with-choices ((param choice-type) &rest body)
  (alexandria:with-gensyms (choice-point)
	`(let ((,choice-point 
			 (make-instance 'choice-point
							:p ,param
							:choices ,(cond ((and (listp choice-type)
												  (eql (first choice-type) :member))
											 `(list ,@(rest choice-type)))
											(t (error "Unknow choice type ~a, (:member ...) are only allowed"
													  choice-type)))
							:old-system (duplicate-system *system*)
							:parent-choice-point *choice-point*)))
	   
	   (let ((*choice-point* ,choice-point))
		 (setf (slot-value ,choice-point 'body)
			   (alexandria:named-lambda with-choice-point ()
				 ,@body))
		 (failed)))))

;;; EXAMPLES


(defun choice-ex1()
  "Choose such value of sh that, the finally codition is satisfied"
  (with-new-system ()
	  (with-parameters (sh q)
		(with-choices (sh (:member :sq :rect :circle))
					  
		  (if (eql (value sh) :sq)
			  (satisfy (- q 10)))
		  
		  (if (eql (value sh) :rect)
			  (satisfy (- q 5)))
		  
		  (if (eql (value sh) :circle)
			  (satisfy (- q 15)))
		  
		  (finally
			(if (< (value q) 12)
				(failed)))
		  
		  (solve-system))

		)))
