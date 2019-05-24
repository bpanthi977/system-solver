;;;; system-solver.lisp

(in-package #:system-solver)

(defclass parameter ()
  ((name :initarg :name)
   (value :initarg :value :accessor value)
   (relations :initarg :relations :accessor relations)
   (dependencies :initarg :dependencies :accessor dependencies)))

(defclass relation ()
  ((params :initarg :params :accessor params)
   (implicit :initarg :implicit)
   (name :initarg :name :type 'string :initform "")))

(defmethod solve ((var symbol) (r relation) &rest params)
  (declare (ignorable params))
  (format t "Don't know how to solve for ~a using relation ~a (~a)" var r (slot-value r 'name)))

(defmacro let-properties (params values &rest body)
  "Generate let bindings for a property list values of the form params"
  (alexandria:once-only (values)
    (labels ((let-form (params result)
	       (if params 
		   (progn (push `(,(second params) (getf ,values ,(first params)))
				result)
			  (let-form (cddr params) result))
		   result)))
      `(let ,(let-form params nil)
	 ,@body))))

(defun lambda-params (params)
  (labels ((collect (params result)
	     (if params
		 (progn (push (second params) result)
			(push (alexandria:make-keyword (second params)) result)
			(collect (cddr params) result))
		 result)))
    (collect params nil)))

(defmacro define-component (name &rest body)
  (let ((parameter-list (getf body :parameters nil))
	(relations-list (getf body :relations nil)))
    (alexandria:with-gensyms (var params) 
      `(progn
	 (defclass ,name ()
	   ,(mapcar (lambda (slot)
		      (if (listp slot)
			  (progn (setf (getf (rest slot) :initarg) (alexandria:make-keyword (first slot)))
				 slot)
			  (list slot :initarg (alexandria:make-keyword slot))))
		    parameter-list))
	 (setf (get ',name 'parameters) ',parameter-list)
	 (setf (get ',name 'relations) nil)
	 ,@(mapcar (lambda (relation)
		     (cond
		       ((getf relation :implicit)
			`(push (make-instance 'relation
					      :implicit ,(getf relation :implicit)
					      :params ',(getf relation :parameters))
			       (get ',name 'relations)))
		       ((getf relation :defined)
			`(push (lambda (,var &rest ,params)
				 (let-properties ,(lambda-params (getf relation :parameters)) ,params
						 (solve ,var ,(getf relation :defined) ,@(getf relation :parameters))))
			       (get ',name 'relations)))))
		   relations-list)))))


;;;; TESTING

(defconstant  +reynolds-number+ (make-instance 'relation
					       :name "Reynold's number definition"
					       :params '(Re v nu D)))

(define-component pipe
    :parameters (Re vel D nu Q A)
    :relations ((:implicit '(- Q (* A vel))
			   :parameters (Q A vel))
		(:defined +reynolds-number+
		    :parameters (:Re Re :v vel :nu nu :D D))))


(defmethod solve ((var (eql 'Re)) (r (eql *reynolds-number*)) &rest params)
  (let-properties (:d d :v v :nu nu) params
		  (* d v (/ nu))))
		  

