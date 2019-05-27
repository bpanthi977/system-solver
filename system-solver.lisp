;;;; system-solver.lisp

(in-package #:system-solver)

(defclass parameter ()
  ((name :initarg :name)
   (value :initarg :value :accessor value)
   (relations :initarg :relations :accessor relations :initform nil)
   (dependencies :initarg :dependencies :accessor dependencies)))

(defclass relation ()
  ((parameters :initarg :params :accessor parameters)
   (parameter-slots :initarg :parameter-slots)
   (implicit :initarg :implicit)
   (name :initarg :name :type 'string :initform "")))

(defclass component ()
  ((name :initarg :name)
   (parameter-slots :initform nil)
   (extra-parameters :initform nil)
   (relations :initform nil)))

(defun convert-slot-values-to-parameters (slots instance)
  (loop for p in slots do
       (if (slot-boundp instance p)
	   (unless (typep (slot-value instance p) 'parameter)
	     (setf (slot-value instance p) (make-instance 'parameter :value (slot-value instance p)
							  :name (string p))))
	   (setf (slot-value instance p) (make-instance 'parameter :name (string p))))))

(defun newton-solve (g g-prime &optional (x0 0) (tolerance 0.0001) (max-iterations 1000))
  "Solve for root of g(x) = 0 within tolerance;
 given derivative of g (g-prime) and initial guess x0"
  (let ((g0 (funcall g x0))
	g-prime0)	
    (loop for i from 0 to max-iterations do
	 (when (< (abs g0) tolerance)
	   (return (values x0 tolerance i)))

	 (setf g-prime0 (funcall g-prime x0))
	 (setf x0 (- x0 (/ g0 g-prime0)))
	 (setf g0 (funcall g x0)))))

(defun newton-solver-general (g &optional (x0 0) (tolerance 0.0001) (max-iterations 1000) (step  0.01))
  (let ((g-derivative #'(lambda (x)
			(/ (- (funcall g (+ x step)) (funcall g x))
			   step))))
    (newton-solve g g-derivative x0 tolerance max-iterations)))

(defun generate-solvers (implicit vars)
  (loop for v in vars
     for args = (remove 'v vars)
     collect
       `(lambda (,args))))


(defmethod initialize-instance :after ((c component) &key)
  (convert-slot-values-to-parameters (slot-value c 'parameter-slots) c))

(defmethod initialize-instance :after ((r relation) &key)
  (convert-slot-values-to-parameters (slot-value r 'parameter-slots) r)
  (setf (slot-value r 'parameters) (loop for v in (slot-value r 'parameter-slots) collect (slot-value r v))))


(defmethod solve-relation ((var symbol) (r relation))
  (when (slot-boundp r 'implicit)
    (destructuring-bind (pre post) (split-sequence:split-sequence var (slot-value r 'parameter-slots))
      (setf pre (mapcar #'(lambda (x) (value (slot-value r x))) pre)
	    post (mapcar #'(lambda (x) (value (slot-value r x))) post))
      (let* ((r (slot-value r 'implicit))
	     (g (lambda (x)
		  (apply r (concatenate 'list pre (list x) post)))))
	(newton-solver-general g 0)))))

(defun known-parameter-p (parameter)
  "Is the parameter's value known?"
  (slot-boundp parameter 'value))

(defun solve-parameter (p &key traversed-relations traversed-parameters)
  (let* ((relation (a-member-of (relations p)))
	 unknowns)
    (print (slot-value p 'name))
    (when (find relation traversed-relations)
      (format t "already ~a" relation)
      (fail))
    ;; (inspect relation)
    (let ((traversed-relations (cons relation traversed-relations))
	  (traversed-parameters (cons p traversed-parameters)))
      (setf unknowns (remove-if #'known-parameter-p (slot-value relation 'parameters)))
      (if unknowns
	  (progn
	    (loop for unknown in unknowns do
		 (if (find unknown traversed-parameters)
		     (unless (slot-boundp unknown 'value)
		       (setf (value unknown) 0))
		     (progn 
		       (format t "~% solving for ~a" (slot-value unknown 'name))
		       (setf (values traversed-relations traversed-parameters)
			     (solve-parameter unknown
					      :traversed-relations traversed-relations
					      :traversed-parameters traversed-parameters))))))
	  (progn (solve-relation p relation)))
      (values traversed-relations traversed-parameters))))

(defun print-parameters (ps)
  (loop for p in ps  do
       (format t "~%~a = ~a" (slot-value p 'name) (value p))))

(defun print-relation-parameters (r)
  (print (slot-value r 'name))
  (print-parameters (slot-value r 'parameters)))

(defun execute-solution-strategy0 (relations parameters) 
  (loop
     for r in relations
     for p in parameters do
       (print "Parameter")
       (print (slot-value p 'name))
     ;; (inspect r)
       (print-relation-parameters r)
       (solve-relation (nth (position p (slot-value r 'parameters))
			    (slot-value r 'parameter-slots))
		       r)
       (format t "~%~a = ~a" (slot-value p 'name) (value p))))

(defun execute-solution-strategy (unknown relations parameters
				  &optional (initial-guess 0) (tolerance 0.000001))
  (loop for count from 0 to 100
     with previous-value = (setf (value unknown) initial-guess) do
       (execute-solution-strategy0 relations parameters)
       (print previous-value)
       (when (< (abs (- previous-value (value unknown))) tolerance)
	 (return (value unknown)))
       (setf previous-value (value unknown))
     finally (progn (print "Iterated 10 times, not within tolerance")
		    (value unknown))))

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
    (alexandria:with-gensyms (instance relation-var) 
      `(progn
	 (defclass ,name (component)
	   ,(append
	     (list `(parameter-slots :initform ',parameter-list))
	     (mapcar #'param->slot parameter-list)))
	 ,(when relations-list
	    `(defmethod initialize-instance :after ((,instance ,name) &key)
			(with-slots ,parameter-list ,instance		       
			  ,@(mapcar (lambda (relation)
				      (let* ((parameters (getf relation :parameters))
					     (param-vars (plist-values parameters)))
					(cond
					  ((getf relation :relation)
					   `(let ((,relation-var (make-instance ',(getf relation :relation) ,@parameters)))
					      (push ,relation-var (slot-value ,instance 'relations))
					      ,@(loop for p in param-vars
						   collect `(push ,relation-var (slot-value (slot-value ,instance ',p) 'relations))))))))
				    relations-list))))))))

(defmacro define-relation (name &rest body)
  (let ((parameter-list (getf body :parameters nil))
	(implicit (getf body :implicit nil))
	(long-name (getf body :name nil)))
    `(defclass ,name (relation)
       ,(append
	 (list `(parameter-slots :initform ',parameter-list :allocation :class))
	 (when long-name `((name :initform ,long-name :allocation :class)))
	 (when implicit `((implicit :initform (lambda ,parameter-list ,implicit) :allocation :class)))
	 (mapcar #'param->slot parameter-list)))))


(defclass implicit-test (relation)
  ((x :initarg :x)
   (y :initarg :y)
   (implicit :initform (lambda (x y) (+ x y)))
   (parameter-slots :initform '(x y))))  

;;;; TESTING
(define-component pipe
    :parameters (Re vel D nu Q A r hf p1 p2)
    :relations ((:relation pipe-discharge
			   :parameters (:Q q :v vel :d d))
		(:relation reynolds-number
			   :parameters (:Re Re :v vel :nu nu :D D))
		(:relation head-loss
			   :parameters (:hf hf :r r :q q))
		(:relation head-loss-2
			   :parameters (:hf hf :p1 p1 :p2 p2))))

(defun tt ()
  (let* ((p1 (make-instance 'pipe :vel 1 :d 2 :nu 0.03))
	 (unknown (slot-value p1 'Q)))
    (one-value
     (multiple-value-bind (relations parameters) (solve-parameter unknown)
       (print relations)
       (print-parameters parameters)
       (execute-solution-strategy unknown relations parameters)))))

(defmethod update-parameter-names ((c component))
  (loop for x in (slot-value c 'parameter-slots)
     for param = (slot-value c x)
     with component-name = (slot-value c 'name) do
       (setf (slot-value param 'name) (concatenate 'string component-name "." (slot-value param 'name)))))


(defun t2 ()
  (let* ((p1 (make-instance 'pipe :name "p1" :r 15938 :p1 24))
	 (p2 (make-instance 'pipe :name "p2" :r 83565 :p1 8))
	 (p3 (make-instance 'pipe :name "p3" :r 170014 :p1 0))
	 (unknown (slot-value p1 'q))
	 (discharge-relation (make-instance 'continuity)))
    (update-parameter-names p1)
    (update-parameter-names p2)
    (update-parameter-names p3)
    (add-discharge-connection (slot-value p1 'q) discharge-relation)
    (add-discharge-connection (slot-value p2 'q) discharge-relation)
    (add-discharge-connection (slot-value p3 'q) discharge-relation)
    (make-instance 'equal-pressure :p1 (slot-value p1 'p2) :p2 (slot-value p2 'p2))
    (make-instance 'equal-pressure :p1 (slot-value p3 'p2) :p2 (slot-value p2 'p2))

    (one-value
     (multiple-value-bind (relations parameters) (solve-parameter unknown)
       (print relations)
       (execute-solution-strategy unknown relations parameters)
       ))))


(defclass reynolds-number (relation)
  ((name :initform "Definition of Reynolds Number" :allocation :class)
   (parameter-slots :initform '(Re v nu D) :allocation :class)
   (Re :initarg :re :initform (make-instance 'parameter))
   (v :initarg :v :initform (make-instance 'parameter))
   (nu :initarg :nu :initform (make-instance 'parameter))
   (D :initarg :D :initform (make-instance 'parameter))))

(defmethod initialize-instance :after ((i reynolds-number) &key)
  (with-slots (Re v nu D) i
    (setf (slot-value i 'parameters) (list Re v nu D))
    (setf (slot-value i 'parameter-slots) '(Re v nu D))))


(defmethod solve-relation (var (r reynolds-number))
  (with-slots (Re v nu D) r
    (setf (value (slot-value r var))
	  (cond ((eql var 'Re)
		 (* (value d) (value v) (/ (value nu))))
		((eql var 'v)
		 (* (value Re) (value nu) (/ (value d))))
		((eql var 'd)
		 (* (value Re) (value nu) (/ (value v))))
		((eql var 'nu)
		 (* (value v) (value d) (/ (value Re))))))))

(defclass pipe-discharge (relation)
  ((name :initform "Discharge through pipe" :allocation :class)
   (parameter-slots :initform '(Q v d))
   (Q :initarg :Q :initform (make-instance 'parameter))
   (v :initarg :v :initform (make-instance 'parameter))
   (d :initarg :d :initform (make-instance 'parameter))))

(defmethod initialize-instance :after ((i pipe-discharge) &key)
  (with-slots (Q v d parameters parameter-slots) i
    (setf parameters (list Q v d))))

(defmethod solve-relation ((var (eql 'Q)) (r pipe-discharge))
  (with-slots (q v d) r
    (setf (value q) (* (value v) (expt (value d) 2) pi 1/4))))

(defmethod solve-relation ((var symbol) (r pipe-discharge))
  (with-slots (q v d) r 
    (cond ((eql var 'd)
	   (setf (value d) (sqrt (* (/ (value q) (value v) pi) 4))))
	  ((eql var 'v)
	   (setf (value v) (/ (value q) (* (expt (value d) 2) pi 1/4)))))))

(defclass head-loss (relation)
  ((name :initform "Head loss depending on discharge" :allocation :class)
   (parameter-slots :initform '(r Q hf))
   (r :initarg :r :initform (make-instance 'parameter))
   (Q :initarg :Q :initform (make-instance 'parameter))
   (hf :initarg :hf :initform (make-instance 'parameter))))

(defmethod initialize-instance :after ((i head-loss) &key)
  (with-slots (r Q hf parameters parameter-slots) i
    (setf parameters (list r Q hf))))

(defmethod solve-relation ((var symbol) (r head-loss))
  (with-slots (r q hf) r
    (cond ((eql var 'hf)
	   (setf (value hf) (* (signum (value q)) (value r) (expt (value q) 2))))
	  ((eql var 'q)
	   (setf (value q) (* (signum (value hf)) (sqrt (abs (/ (value hf) (value r))))))))))

(define-relation head-loss-2
    :parameters (hf p1 p2)
    :implicit (- hf (+ p1 p2))
    :name "Head loss depending on pressure difference")

(defmethod solve-relation ((var symbol) (r head-loss-2))
  (with-slots (p1 p2 hf) r
    (cond ((eql var 'p1)
	   (setf (value p1) (+ (value hf) (value p2))))
	  ((eql var 'p2)
	   (setf (value p2) (- (value p1) (value hf))))
	  ((eql var 'hf)
	   (setf (value hf) (- (value p1) (value p2)))))))	   

(defclass continuity (relation)
  ((name :initform "Continuity at a junction" :allocation :class)
   (parameters :initform nil)
   (parameter-slots :initform nil)))

(defmethod add-discharge-connection ((p parameter) (c continuity))
  (with-slots (parameters parameter-slots) c
    (push p parameters)
    (push c (relations p))
    (let ((f (first parameter-slots)))
      (push (if f (1+ f) 1) parameter-slots))))

(defmethod solve-relation ((var number) (r continuity))
  (with-slots (parameters) r
    ;; (print var)
    ;; (print parameters)
    (loop for p in parameters
       with req = (nth (1- var) (reverse parameters))
       with sum = 0 do
       ;;  (print (slot-value p 'name))
       ;; (print (value p))
	 (unless (eql p req)
	   (incf sum (value p)))
       ;; (print sum)
       finally
	 (setf (value req) (- sum)))))

(defclass equal-pressure (relation)
  ((name :initform "Equal pressure at two point or a junction" :allocation :class)
   (parameter-slots :initform '(p1 p2))
   (p1 :initform (make-instance 'property) :initarg :p1)
   (p2 :initform (make-instance 'property) :initarg :p2)))

(defmethod initialize-instance :after ((i equal-pressure) &key)
  (with-slots (p1 p2 parameters parameter-slots) i
    (push i (relations p2))
    (push i (relations p1))))

(defmethod solve-relation ((var symbol) (r equal-pressure))
  (with-slots (p1 p2) r
    (cond ((eql var 'p1)
	   (setf (value p1) (value p2)))
	  ((eql var 'p2)
	   (setf (value p2) (value p1))))))

;; (time (t2)) => 0.020587102
;; Evaluation took:
  ;; 0.026 seconds of real time
  ;; 0.020870 seconds of total run time (0.019109 user, 0.001761 system)
  ;; 80.77% CPU
  ;; 60,385,688 processor cycles
  ;; 950,896 bytes consed
