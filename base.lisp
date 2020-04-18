(in-package :system-solver)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PARAMETERS AND RELATIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass parameter ()
  ((name :initarg :name :accessor name)
   (value :initarg :value :accessor value)
   (relations
	:initarg :relations :accessor relations :initform nil
	:documentation "The relations this parameter can be solved from")
   (solvable-state
	:initform :unknown :accessor solvable-state
	:documentation "The state of the value of the parameter during search for solution strategy 
:unknown :unsolvable :solvable")))

(defun known-parameter-p (parameter)
  "Is the parameter's value known?"
  (let ((solved? (or (eql (solvable-state parameter) :solved)
					 (eql (solvable-state parameter) :known))))
	(if solved?
		(progn 
		  (unless (and (slot-boundp parameter 'value)
						(value parameter))
			(error "Solved parameter ~a doesn't have value ?? why" parameter))
		  (value parameter)))))

(defmethod print-object ((p parameter) stream)
  (format stream "<#P ~a ~a ~a>"
		  (if (slot-boundp p 'name) (slot-value p 'name) "unnamed")
		  (if (eql (solvable-state p) :unknown)
			  (if (slot-boundp p 'value) "≈" "")
			  "=")
		  (if (slot-boundp p 'value) (value p) (solvable-state p))))

(defmethod initialize-instance :after ((p parameter) &key)
  "Add this to *system* if available"
  (if (and (slot-boundp p 'value) (value p)
		   (eql (solvable-state p) :unknown))
	  (setf (solvable-state p) :known))
  (when *system*
	(add-parameter% p *system*)))

(defclass relation ()
  ((parameters
	:accessor parameters :initform nil :initarg :parameters
	:documentation "The parameter objects of this relation
parameter instances and this list are not to be changed once initialized")
   
   (unsolvable-parameters
	:initarg :unsolvable-parameters :initform nil
	:documentation "The parameters that appear in this relation but cannot be solved using this relation")
   
   (name
	:initarg :name :type 'string :initform ""))
  (:documentation "Relation object denote relation between parameters. 
They are the links that connect nodes of parameters in the web of parameters and relations"))

(defmethod initialize-instance :after ((r relation) &key)
  "Add this relation to parameters provided. (except for unsolvable parameters)
and also add it to *system* if available"
  (when *system* (add-relation r *system*))
  (with-slots (unsolvable-parameters parameters) r
	(loop for p in (set-difference parameters unsolvable-parameters :test #'eql) do
		 (pushnew r (slot-value p 'relations)))))	

(defmethod print-object ((r relation) s)
  (format s "<#~a ~a>" (slot-value r 'name) (slot-value r 'parameters)))

(defmethod remove-relation ((r relation) (p parameter))
  (setf (relations p) (remove r (relations p))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IMPLICIT RELATION AND COMPOSITE RELATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass implicit-relation (relation)
  ((implicit
	:initarg :implicit
	:documentation "A function with arguments same as parameters of the relation"))
  
  (:documentation "An implicit function with the arguments"))

(defmethod solve-relation ((p parameter) (r implicit-relation))
  ;; This assumes that parameter order is unaltered after implicit relation is created
  (when (slot-boundp r 'implicit)
    (destructuring-bind (pre post) (split-sequence:split-sequence p (parameters r))
      (setf pre (mapcar #'value pre)
			post (mapcar #'value post))
      (let*  ((implicit (slot-value r 'implicit))
			  (g (lambda (x)
				   (apply implicit (concatenate 'list pre (list x) post)))))
		(setf (value p) (newton-solver-general g 1))))))

(defmethod eval-relation ((r implicit-relation))
  (if (slot-boundp r 'implicit)
      (let ((params (mapcar #'value (parameters r))))
		(apply (slot-value r 'implicit) params))
      (error "Cannot evaluate relation ~a" r)))

;; For safety purpose only. Ensures the assumption in above solve-relation and eval-relation methods
;; that the parameters are unaltered
(defmethod (setf parameters) (value (r relation))
  (error "Can't edit parameters after creating relation")
  (setf (slot-value r 'parameters) value))

(defclass composite-relation (relation)
  ((originalr :initarg :originalr)
   (ep :initarg :ep) 
   (es-r :initarg :es-r) 
   (sp :initarg :sp) 
   (e->s :initarg :e->s)
   (s->e :initarg :s->e))
  (:documentation "ep = eliminated parameter
es-r = relation used to eliminate
sp = substitution parameter"))

(defmethod initialize-instance :after ((cr composite-relation) &key)
  (setf (slot-value cr 'parameters) (union (list (slot-value cr 'sp))
										   (remove (slot-value cr 'ep)
												   (parameters (slot-value cr 'originalr))))))

(defmethod print-object  ((cr composite-relation) s)
  (with-slots (ep sp originalr ep-r) cr
    (format s "<# CR ep:~a sp:~a (~a)>"
			ep sp (parameters cr))))

(defmethod solve-relation ((p parameter) (cr composite-relation))
  "Solve for parameter p using composed-relation cr"
  (with-slots (ep sp originalr) cr
    (if (eq p sp)
		(progn
		  (solve-relation ep originalr)
		  (setf (value sp) (funcall (slot-value cr 'e->s) (value ep))))
		(progn
		  (setf (value ep) (funcall (slot-value cr 's->e) (value sp)))
		  (solve-relation p originalr)))))

(defmethod eval-relation ((cr composite-relation))
  (with-slots (s->e originalr ep sp) cr
    (setf (value ep) (funcall s->e (value sp)))
    (eval-relation originalr)))

(defmethod create-composite-relation (rs (ep parameter) (es-r relation) (sp parameter))
  "Replace all relations rs of ep (eliminated parameter) by composing new relations wrt sp (substituted parameter) using es-r; 
Thus n(unknown_params(r)) = 2"
  (let* ((p_0 nil)
		 (o_0 nil)
		 (p->o (lambda (p)
				 (if (and p_0
						  (= p p_0))
					 o_0
					 (progn (solve-relation sp es-r)
							(setf p_0 p
								  o_0 (value sp))))))
		 (o->p (lambda (o)
				 (if (and o_0
						  (= o o_0))
					 p_0
					 (progn (solve-relation ep es-r)
							(setf o_0 o
								  p_0 (value ep))))))
		 cr crs)
    (loop for r in rs do
		 (if (eq r es-r)
			 (setf (relations sp) (remove es-r (relations sp)))
			 (progn 
			   (setf cr (make-instance 'composite-relation :ep ep :sp sp :es-r es-r
									   :e->s p->o :s->e o->p :originalr r))
			   (loop for p in (unknowns r)
				  unless (eq p ep) do
					(setf (relations p) (cons cr (remove r (relations p)))))
			   (push cr crs))))
    (setf (relations sp) (union (relations sp) crs))
    crs))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TOOLS FOR TESTING 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod test-relation ((r relation) &optional (tolerance 0.01))
  "Test if the implicit realtion given, 
solve-relation and eval-relation methods agree with each other or not"
  (with-slots (parameters) r
    (loop for p in parameters
       with values = nil do
		 (loop for p2 in parameters
			unless (eq p2 p) do 
			  (setf (value p2) (- (random 1000) 500)))
		 (solve-relation p r)
		 (unless (< (abs (eval-relation r)) tolerance)
		   (print parameters)
		   (error "Faulty realtion solver or evaluator, after solving for ~a in ~a" p r))
		 (setf values (mapcar #'value parameters))
		 (loop for p2 in parameters
			for index from 0 do
			  (solve-relation p2 r)
			  (unless (< (abs (- (nth index values) (value p2))) tolerance)
				(error "Faulty solver, solving for ~a in ~a" p2 r))))))

(defun test-all-relations-in-composite-relation (cr &optional (tolerance 0.001))
  (labels ((collect-r (cr &optional rs)
			 (if (typep cr 'composite-relation)
				 (cons (slot-value cr 'es-r) (collect-r (slot-value cr 'originalr)))
				 (cons cr rs))))
    (loop for r in (collect-r cr) do
		 (test-relation r tolerance))))
