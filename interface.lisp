(in-package :system-solver)


(defclass relation-managed (relation)
  ((parameter-slots
	:initarg :parameter-slots :initform nil
	:documentation "The slots provided by the class that inherits this"))
  
  (:documentation "The relation whose initialization is managed.
Instance of this class have no meaning; instead inherit this class with proper parameter-slots.
Subclass of this class have slots corresponding to each parameter, and initialization arguments 
passed to those slots are auto converted parameter type (if required) and also added to the parameters list
Also solve-relation is specialized to accept parameter slot name for convenience"))

(defun sanitize-parameter-slots (slots instance)
  "Convert values provided during intialization to parameter type for given slots of instance"
  (loop for p in slots do
       (if (slot-boundp instance p)
		   (unless (typep (slot-value instance p) 'parameter)
			 (setf (slot-value instance p) (make-instance 'parameter
														  :value (slot-value instance p)
														  :name (string p))))
		   (setf (slot-value instance p) (make-instance 'parameter :name (string p))))))

(defmethod initialize-instance :before ((r relation-managed) &rest initargs)
  "Sanitize slot-values and create parameter list"
  (with-slots (parameter-slots parameters) r
	;; sanitize slot values
	(loop for p in parameter-slots
	   for v  = (getf initargs (intern (symbol-name p) :keyword)) do
		 (setf (slot-value r p)
			   (if v
				   (if (typep v 'parameter)
					   v
					   (make-instance 'parameter :value v :name (string p)))
				   (make-instance 'parameter :name (string p)))))
  ;; create parameter list
	(setf parameters (loop for s in parameter-slots collect (slot-value r s)))))

(defmethod solve-relation ((var symbol) (r relation-managed))
  "Solve relation for given parameter name"
  (let ((p (position var (slot-value r 'parameter-slots))))
	(unless p
	  (error "Unknown parameter ~a to solve for in realtion ~a" var r))
	(solve-relation (nth p (slot-value r 'parameters)) r)))

(defclass relation-implicit-managed (implicit-relation relation-managed)
  ()
  (:documentation "A implicit relation that is managed"))

(defmacro with-parameters (paramlist &rest body)
  `(let ,(loop for p in paramlist
			collect (if (listp p)
						`(,(first p) (make-instance 'parameter :value ,(second p) :name ,(symbol-name (first p))))
						`(,p (make-instance 'parameter :name ,(symbol-name p)))))
     ,@body))

(defmacro smart-parse-implicit-relation (&whole exp (operator &rest operands))
  "Parse relations like (= a (- b 2) (+ c 2)) , (- a b) as implicit relation"
  (labels ((collect-local-vars (exp vars)
			 (force-output)
			 (loop for e in (if (listp exp) (rest exp) (list exp)) do
			   (if (atom e)
				   (if (and (symbolp e)
							(let ((str (symbol-name e)))
							  (or (not (alexandria:starts-with #\* str))
								  (not (alexandria:ends-with #\* str)))))
					   (push e vars))
				   (setf vars (collect-local-vars e vars))))
			 vars)
		   (relation-for-= (operand1 operand2)
			 (let ((parameters (union (collect-local-vars operand1 nil)
									  (collect-local-vars operand2 nil))))
			   `(make-instance 'implicit-relation
							   :implicit (lambda ,parameters
										   (- ,operand1 ,operand2))
							   :parameters (list ,@parameters)))))
	(cond ((eql operator '=)
		   `(progn
			  ,@(loop with first-operand = (first operands)
				   for operand in (rest operands)
				   collect
					 (relation-for-= first-operand operand))))
		  (t
		   (let ((parameters (remove-duplicates (collect-local-vars (second exp) nil))))
			 `(make-instance 'implicit-relation
							 :parameters (list ,@parameters)
							 :implicit (lambda ,parameters
										 ,(second exp))))))))

(defmacro satisfying-relations (&rest forms)
  "make-instance of relations"
  `(list ,@(loop for r in forms do
				(assert (listp r))
			  collect (cond ((eql (first r) 'lambda)
							 `(make-instance 'system-solver::implicit-relation
											 :implicit ,r
											 :parameters (list ,@(second r))
											 :name ,(if (and (third r) (stringp (third r))) (third r) "")))
							((find (first r) '(= + - * /))
							 `(smart-parse-implicit-relation ,r))
							(t `(make-instance ',(first r) ,@(rest r)))))))

(defmacro satisfy (&rest forms)
  "same as satisfying-relations"
  `(satisfying-relations ,@forms))

(defclass component ()
  ((name :initarg :name :accessor name)
   (parameter-slots :initform nil)
   (relations :initform nil)))
   

(defmethod update-parameter-names ((c component))
  (loop for x in (slot-value c 'parameter-slots)
     for param = (slot-value c x)
     with component-name = (slot-value c 'name) do
       (setf (slot-value param 'name) (concatenate 'string component-name "." (slot-value param 'name)))))

(defmethod initialize-instance :after ((c component) &key)
  (sanitize-parameter-slots (slot-value c 'parameter-slots) c)
  (when (slot-boundp c 'name)
    (update-parameter-names c)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun param->slot (param)
    (setf param (alexandria:ensure-list param))
	(let ((result (list (first param)
						:initarg (alexandria:make-keyword (first param)))))
	  (when (second param)
		(alexandria:appendf result (list :initform (second param))))

	  result)))
	

(defmacro define-component (name &rest body)
  ":parameters 
:relations"
  (let* ((parameter-list (getf body :parameters nil))
		(parameters (mapcar (lambda (x) (if (listp x) (first x) x)) parameter-list))
		(relations-list (getf body :relations nil)))
    (alexandria:with-gensyms (instance) 
      `(progn
		 (defclass ,name (component)
		   ,(append
			 (list `(parameter-slots :initform ',parameters))
			 (mapcar #'param->slot parameter-list)))
		 ,(when relations-list
			`(defmethod initialize-instance :after ((,instance ,name) &key)
						(with-slots ,parameters ,instance
						  (nconc (slot-value ,instance 'relations)
								 (satisfying-relations ,@relations-list)))))))))

(defmacro define-relation (name &rest body)
  ":parameters 
:implicit
:name"
  (let ((parameter-list (getf body :parameters nil))
		(implicit (getf body :implicit nil))
		(superclass (getf body :class nil))
		(long-name (getf body :name nil)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (defclass ,name (relation-implicit-managed ,@superclass)
		 ,(append
		   (list `(parameter-slots :initform ',parameter-list :allocation :class))
		   (when long-name `((name :initform ,long-name :allocation :class)))
		   (when implicit `((implicit :initform (lambda ,parameter-list ,implicit))))
		   (mapcar #'param->slot parameter-list))))))

;; (defmacro define-block (name parameter-list &rest body)
;;   "Define a relations/design block to be included elsewhere"
;;   `(define-component ,name
;; 	 :parameters ,parameter-list
;; 	 :body (progn ))


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
	
;;;
;;  EXAMPLES
;;;


(defun ex-interface1()
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

(defun ex-interface2()
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
