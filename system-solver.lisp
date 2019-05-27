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

(defmethod update-parameter-names ((c component))
  (loop for x in (slot-value c 'parameter-slots)
     for param = (slot-value c x)
     with component-name = (slot-value c 'name) do
       (setf (slot-value param 'name) (concatenate 'string component-name "." (slot-value param 'name)))))

(defmethod initialize-instance :after ((c component) &key)
  (convert-slot-values-to-parameters (slot-value c 'parameter-slots) c)
  (update-parameter-names c))

(defmethod initialize-instance :after ((r relation) &key)
  (convert-slot-values-to-parameters (slot-value r 'parameter-slots) r)
  (setf (slot-value r 'parameters) (loop for v in (slot-value r 'parameter-slots)
				      for p = (slot-value r v) do
					(pushnew r (slot-value p 'relations) :test #'eql)
					collect p)))


(defmethod solve-relation ((var symbol) (r relation))
  (print "Solving general relation")
  (print-parameters (slot-value r 'parameters ))
  ;; (inspect r)
  (when (slot-boundp r 'implicit)
    (destructuring-bind (pre post) (split-sequence:split-sequence var (slot-value r 'parameter-slots))
      (setf pre (mapcar #'(lambda (x) (value (slot-value r x))) pre)
	    post (mapcar #'(lambda (x) (value (slot-value r x))) post))
      (let* ((implicit (slot-value r 'implicit))
	     (g (lambda (x)
		  (apply implicit (print (concatenate 'list pre (list x) post))))))
	(setf (value (slot-value r var)) (newton-solver-general g 0))))))

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
    (print (slot-value relation 'name))
    (print-parameters (slot-value relation 'parameters))
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
       (format t "~%~a = ~a" (slot-value p 'name) (if (slot-boundp p 'value) (value p) "Unbound"))))

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
    (alexandria:with-gensyms (instance) 
      `(progn
	 (defclass ,name (component)
	   ,(append
	     (list `(parameter-slots :initform ',parameter-list))
	     (mapcar #'param->slot parameter-list)))
	 ,(when relations-list
	    `(defmethod initialize-instance :after ((,instance ,name) &key)
			(with-slots ,parameter-list ,instance		       
			  ,@(mapcar (lambda (relation)
					(cond
					  ((getf relation :relation)
					   `(push (make-instance ',(getf relation :relation)
								 ,@(getf relation :parameters))
						  (slot-value ,instance 'relations)))))					  
				    relations-list))))))))

(defmacro define-relation (name &rest body)
  (let ((parameter-list (getf body :parameters nil))
	(implicit (getf body :implicit nil))
	(long-name (getf body :name nil)))
    `(defclass ,name (relation)
       ,(append
	 (list `(parameter-slots :initform ',parameter-list :allocation :class))
	 (when long-name `((name :initform ,long-name :allocation :class)))
	 (when implicit `((implicit :initform (lambda ,parameter-list ,implicit))))
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



(defun t2 ()
  (let* ((p1 (make-instance 'pipe :name "p1" :r 15938 :p1 24))
	 (p2 (make-instance 'pipe :name "p2" :r 83565 :p1 8))
	 (p3 (make-instance 'pipe :name "p3" :r 170014 :p1 0))
	 (unknown (slot-value p1 'q))
	 (discharge-relation (make-instance 'continuity)))
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


(define-relation reynolds-number
    :parameters (Re v nu D)
    :implicit (- Re (* v D (/ nu)))
    :name "Definition of Reynolds Number")

(define-relation pipe-discharge 
    :name "Discharge through pipe"
    :parameters (Q v d)
    :implicit (- Q (* v (/ pi 4) (expt d 2))))

(defun t3 ()
  (let* ((r (make-instance 'pipe-discharge :d 2 :v 1 :q 12)))

    (solve-relation 'q r)))
    ;; (one-value
    ;;  (multiple-value-bind (relations parameters) (solve-parameter unknown)
    ;;    (print relations)
    ;;    (execute-solution-strategy unknown relations parameters)
    ;;    ))))

(define-relation head-loss
    :name  "Head loss depending on discharge" 
    :parameters (r Q hf)
    :implicit (- hf (* r (expt Q 2))))

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
    (loop for p in parameters
       with req = (nth (1- var) (reverse parameters))
       with sum = 0 do
	 (unless (eql p req)
	   (incf sum (value p)))
       finally
	 (setf (value req) (- sum)))))

(define-relation equal-pressure
    :name "Equal pressure at two point or a junction"
    :parameters (p1 p2)
    :implicit (- p1 p2))

;; (time (t2)) => 0.020587102
;; Evaluation took:
  ;; 0.026 seconds of real time
  ;; 0.020870 seconds of total run time (0.019109 user, 0.001761 system)
  ;; 80.77% CPU
  ;; 60,385,688 processor cycles
  ;; 950,896 bytes consed

;; ;; Evaluation took:
;;   0.053 seconds of real time
;;   0.054793 seconds of total run time (0.054793 user, 0.000000 system)
;;   103.77% CPU
;;   120,340,747 processor cycles
;;   3,374,416 bytes consed
  
;; 0.020587102
