(in-package :system-solver)

(defmacro with-parameters (paramlist &rest body)
  `(let ,(loop for p in paramlist
	    collect (if (listp p)
			`(,(first p) (make-instance 'parameter :value ,(second p) :name ,(symbol-name (first p))))
			`(,p (make-instance 'parameter :name ,(symbol-name p)))))
     ,@body))
 
(defmacro satisfying-relations (&rest forms)
  "make-instance of relations"
  `(list ,@(loop for r in forms do
		 (assert (listp r))
		 collect (cond ((eql (first r) 'lambda)
				`(make-instance 'system-solver::implicit-relation
						:implicit ,r
						:parameters (list ,@(second r))))
			       (t `(make-instance ',(first r) ,@(rest r)))))))

(defclass component ()
  ((name :initarg :name)
   (parameter-slots :initform nil)
   (extra-parameters :initform nil)
   (relations :initform nil)))

(defmethod update-parameter-names ((c component))
  (loop for x in (slot-value c 'parameter-slots)
     for param = (slot-value c x)
     with component-name = (slot-value c 'name) do
       (setf (slot-value param 'name) (concatenate 'string component-name "." (slot-value param 'name)))))

(defmethod initialize-instance :after ((c component) &key)
  (slot-values->parameters (slot-value c 'parameter-slots) c)
  (when (slot-boundp c 'name)
    (update-parameter-names c)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun lambda-params (params)
    (labels ((collect (params result)
	       (if params
		   (progn (push (second params) result)
			  (push (alexandria:make-keyword (second params)) result)
			  (collect (cddr params) result))
		   result)))
      (collect params nil)))

  (defun plist-values (plist)
    (labels ((rec (plist)
	       (when plist
		 (cons (second plist) (rec (cddr plist))))))
      (rec plist)))

  (defun param->slot (param)
    (setf param (alexandria:ensure-list param))
    (setf (getf (rest param) :initarg) (alexandria:make-keyword (first param)))
    param))

(defmacro define-component (name &rest body)
  (let ((parameter-list (getf body :parameters nil))
	(relations-list (getf body :relations nil)))
    (alexandria:with-gensyms (instance) 
      `(progn
	 (defclass ,name (component)
	   ,(append
	     (list `(parameter-slots :initform ',parameter-list))
	     (mapcar #'param->slot parameter-list)))
	 ,(when relations-list
	    `(defmethod initialize-instance :after ((,instance ,name) &key)
			(with-slots ,parameter-list ,instance
			  (nconc (slot-value ,instance 'relations)
				 (satisfying-relations ,@relations-list)))))))))

(defmacro define-relation (name &rest body)
  (let ((parameter-list (getf body :parameters nil))
	(implicit (getf body :implicit nil))
	(long-name (getf body :name nil)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (defclass ,name (relation)
	 ,(append
	   (list `(parameter-slots :initform ',parameter-list :allocation :class))
	   (when long-name `((name :initform ,long-name :allocation :class)))
	   (when implicit `((implicit :initform (lambda ,parameter-list ,implicit))))
	   (mapcar #'param->slot parameter-list))))))
