(in-package :system-solver)

(defclass parameter ()
  ((name :initarg :name)
   (value :initarg :value :accessor value)
   (relations :initarg :relations :accessor relations :initform nil)
   (solvable-state :initform :unknown :accessor solvable-state)))

(defun known-parameter-p (parameter)
  "Is the parameter's value known?"
  (and (slot-boundp parameter 'value)
       (value parameter)))

(defmethod print-object ((p parameter) stream)
  (format stream "<#P ~a = ~a>"
	  (if (slot-boundp p 'name)
	      (slot-value p 'name)
	      "unnamed")
	  (if (slot-boundp p 'value)
	      (value p)
	      (solvable-state p))))  

(defclass relation ()
  ((parameters :accessor parameters :initform nil) ;; parameters are not to be set during initialization
   (parameter-slots :initarg :parameter-slots :initform nil)
   (implicit :initarg :implicit)
   (unsolvable-parameters :initarg :unsolvable-parameters :initform nil)
   (name :initarg :name :type 'string :initform "")))

(defmethod print-object ((r relation) s)
  (format s "<#~a ~a>" (slot-value r 'name) (slot-value r 'parameters)))

(defun slot-values->parameters (slots instance)
  "Convert values provided during intialization to parameter type"
  (loop for p in slots do
       (if (slot-boundp instance p)
	   (unless (typep (slot-value instance p) 'parameter)
	     (setf (slot-value instance p) (make-instance 'parameter :value (slot-value instance p)
							  :name (string p))))
	   (setf (slot-value instance p) (make-instance 'parameter :name (string p))))))

(defmethod initialize-instance :after ((r relation) &key)
  "Sanitize slot-values, create parameter list, and add this relation to provided parameters"
  (with-slots (parameter-slots parameters) r
    (slot-values->parameters parameter-slots r)
    (setf parameters (loop for s in parameter-slots
			for p = (slot-value r s) do
			  (unless (find s (slot-value r 'unsolvable-parameters))
			    (pushnew r (slot-value p 'relations) :test #'eql))
			collect p))))


(defmethod unknowns ((r relation))
  (remove-if #'known-parameter-p (parameters r)))

(defmethod unknowns ((l list))
  (remove-if #'known-parameter-p l))

(defmethod solve-relation ((var symbol) (r relation))
  "Solve relation for given parameter name "
  (if (slot-boundp r 'implicit)
    (destructuring-bind (pre post) (split-sequence:split-sequence var (slot-value r 'parameter-slots))
      (setf pre (mapcar #'(lambda (x) (value (slot-value r x))) pre)
	    post (mapcar #'(lambda (x) (value (slot-value r x))) post))
      (let* ((implicit (slot-value r 'implicit))
	     (g (lambda (x)
		  (apply implicit (concatenate 'list pre (list x) post)))))
	(setf (value (slot-value r var)) (newton-solver-general g 1))))
    (error "Can't solve for ~a when no implicit relation is present in ~a" var r)))

(defmethod solve-relation ((p parameter) (r relation))
;; This assumes that parameter order is unaltered after implicit relation is created
  (when (slot-boundp r 'implicit)
    (destructuring-bind (pre post) (split-sequence:split-sequence p (parameters r))
      (setf pre (mapcar #'value pre)
  	    post (mapcar #'value post))
      (let*  ((implicit (slot-value r 'implicit))
  	     (g (lambda (x)
  		  (apply implicit (concatenate 'list pre (list x) post)))))
  	(setf (value p) (newton-solver-general g 1))))))

(defmethod eval-relation ((r relation))
;; This also assumes that parameter order is unaltered.
  (if (slot-boundp r 'implicit)
      (let ((params (mapcar #'value (parameters r))))
	(apply (slot-value r 'implicit) params))
      (error "Cannot evaluate relation ~a" r)))

;; For safety purpose only. Ensures the assumption in above solve-relation and eval-relation methods
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

;;
;; Tools for testing 
;;
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
