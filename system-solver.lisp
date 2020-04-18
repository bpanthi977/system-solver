;;;; system-solver.lisp
(in-package #:system-solver)


(defmethod unknowns ((r relation))
  "Unknown parameters of an relation"
  (remove-if #'known-parameter-p (parameters r)))

(defmethod unknowns ((l list))
  (remove-if #'known-parameter-p l))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; System Class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass system ()
  ((parameters
	:accessor parameters :initarg :parameters :initform nil 
	:documentation "Parameters that are currently on processsing; active parameters")
   (relations 
	:accessor relations :initarg :relations :initform nil 
	:documentation "Relations used that are directly in use")
   (solved-parameters
	:accessor solved-parameters :initform nil 
	:documentation "Solved Parameters")
   (eliminated-parameters
	:initform nil
	:documentation "Parameters eliminated by substitution")
   (original-parameters
	:initform nil :initarg :original-parameters
	:documentation "Parameters that were asked to be solved")
   (solution-finalizer
	:initform nil :initarg :solution-finalizer
	:documentation "Function that will be run on completion of *first* solve-system command
*first* => The finalizer is set to nil before running the set finalizer so, that another solve-system 
            within the finalizer doesn't cause an infinite recursion")
   (choice-points
	:initform nil :accessor choice-points
	:documentation "Choice points are functions used for backtracking a design
Once a choice point is established, changes to state of system (parameters, relations, solution-finalizer) is 
restored to its original value when the choice point is revisited.")))


(defmethod initialize-instance :after ((s system) &key)
  "Separate known (solved) and unknown parameters"
  (with-slots (parameters) s 
    (let ((unknowns (remove-if #'known-parameter-p parameters))
	  (knowns (remove-if-not #'known-parameter-p parameters)))
      (setf (solved-parameters s) knowns
			parameters unknowns))))

(defmethod copy-system ((from system) (to system))
  (loop for slot in '(parameters relations solved-parameters
						 eliminated-parameters original-parameters solution-finalizer
						 choice-points)
		do (setf (slot-value to slot) (slot-value from slot))))
						   

(defmethod remove-parameter% ((p parameter) (s system))
  "Remove active parameter"
  (with-slots (parameters) s
    (setf parameters (remove p parameters))))

;; (defmethod remove-parameter ((p parameter) (s system))
;;   ;;(error "Not implemented! what do you mean? remove-parameter% ?")
;;   (remove-relations% (relations p) s)
;;   (remove-parameter% p s))

(defmethod remove-relations% ((rs list) (s system))
  "Remove active relations"
  (setf (relations s) (set-difference (relations s) rs)))

(defmethod remove-relations ((rs list) (s system))
  "TODO: maybe unravle composite relations composed using rs"
  (with-slots (relations parameters) s
	(loop for r in rs do 
		 (when (find r relations) 
		   (loop for p in (parameters r) do (remove-relation r p))))
	(setf relations (set-difference relations rs))))
		 

(defmethod add-relations ((rs list) (s system))
  "Add relations"
  (setf (relations s) (union (relations s) rs)))

(defmethod add-relation ((r relation) (s system))
  (pushnew r (relations s)))

(defmethod add-parameter% ((p parameter) (s system))
  "TODO: Some check may be required,
like checking if relations are also imported, 
or if p needs to classified into solvable or unsolvable, solved, ... parameter list"
  (pushnew p (parameters s)))

(defmethod print-object ((s system) stream)
  (with-slots (parameters solved-parameters eliminated-parameters relations) s 
    (format stream "~a+~a parameters ~a relations"
	    (length parameters) (+ (length solved-parameters) (length eliminated-parameters)) (length relations))
    (loop for p in parameters do
	 (print p stream))
    (loop for p in solved-parameters do
	 (print p stream))
    (loop for p in eliminated-parameters do
	 (print p stream))))

;;
;; Strategies :)
;;

(screamer::defun find-solvable-system-for-parameter (p &key traversed-relations traversed-parameters)
  "Find a set of relations that can be used to solve a parameter
Also as a side effect set the solvable-state of parameters to :solvable or :unsolvable"
  (when (or (known-parameter-p p) (find p traversed-parameters))
    (return-from find-solvable-system-for-parameter nil))
  (if (eq (solvable-state p) :unknown)
      (setf (solvable-state p) :unsolvable))
  (let* ((relation (screamer:a-member-of (relations p)))
	 unknowns)
    (when (find relation traversed-relations)
      (screamer:fail))
    (let ((traversed-relations (cons relation traversed-relations))
	  (traversed-parameters (cons p traversed-parameters)))
      (setf unknowns (unknowns relation))
      (if unknowns
	  (progn
	    (loop for unknown in unknowns do
		 (unless (find unknown traversed-parameters)
		   (progn 
		     (setf (values traversed-relations traversed-parameters)
			   (find-solvable-system-for-parameter
			    unknown
			    :traversed-relations traversed-relations
			    :traversed-parameters traversed-parameters))))))
	  (progn (solve-relation p relation)))
      (setf (solvable-state p) :solvable)
      (values traversed-relations traversed-parameters))))

(defun find-solvable-system-for (unknowns &optional (type :largest))
  "Searches for the system of relations and parameters that can be used to solve for the unknowns.
Note: if any unknown is unsolvable, no error will be given, it will simply be discarded

type can be either :largest or :smallest, 
:smallest system consists of as few relations as required to solve for the unknowns, 
thus the solution may not be consistent with other relations of the unknowns most likely 
if the relations selected are not independent (linearly or otherwise).

:largest system includes all usable relations that depend on given unknowns. Hence for 
consistent solution when number of relations > number of parameters, :largest system must be choosen."
  (let (relations parameters)
    (macrolet ((find-rs-and-ps (u)
		 `(multiple-value-bind (r p) (find-solvable-system-for-parameter
					     ,u
					     :traversed-parameters parameters
					     :traversed-relations relations)
		   (setf relations (union r relations))
		   (setf parameters (union p parameters)))))
      (ecase type
	(:largest
	 (loop for u in unknowns do 
	      (screamer:all-values (find-rs-and-ps u))))		
	(:smallest
	 (loop for u in unknowns do
	      (screamer:one-value (find-rs-and-ps u)))))
      (make-instance 'system :relations relations :parameters parameters :original-parameters unknowns))))

(defun search-params/rels (params &optional trels tparams)
  "Search all parameters and relations related to given parameters"
  (loop for p in params
     unless (find p tparams) do
       (push p tparams)
       (loop for r in (relations p)
	  unless (find r trels) do
	    (push r trels)
	    (setf (values trels tparams)
		  (search-params/rels (parameters r) trels tparams))))
  (values trels tparams))

(defun simple-solve-parameter (p)
  "Solve for a parameter simply if there exists any one relation where it is the only unknown"
  (loop for r in (relations p)
     for us = (unknowns r)
     when (= (length us) 1)
     do (solve-relation p r)
	(return r)))

(defmethod simple-solve ((s system))
  "Solve a system as far as possible using one-to-one relations, ie without using simultaneous relations"
  (loop for p in (parameters s)
     with solved = 0
     with r = nil do
       (unless (known-parameter-p p)
	 (setf r (simple-solve-parameter p))
	 (when r 
	   (remove-parameter% p s)
	   (push p (slot-value s 'solved-parameters))
	   (remove-relations% (list r) s)
	   (incf solved)))
     finally (when (> solved 0)
	       (simple-solve s))))

(defmethod choose-for-substitution ((r relation))
  "Choose a parameter to be substituted by another one; 
if the relation or other condition is not suitable  choose no "
  ;; if the parameters are related by more than two relations, reject
  (let ((ps (unknowns r)))
    (when (> (length (remove-if-not #'(lambda (r) (find (second ps) (parameters r)))
    				    (relations (first ps))))
    	     1)
      (return-from choose-for-substitution nil))
    (values (first ps) (second ps))))

(defmethod dimensionally-reduce ((s system))
  "Reduce number of parameters and relations in the system by method of substitution"
  (loop for p in (parameters s) do
       (loop for r in (remove-if-not #'(lambda (r)
					 (and (= (length (unknowns r)) 2)
					     (find r (relations s))))
				     (relations p))
	  when (find p (parameters s)) do
	    (multiple-value-bind (p other)
		(choose-for-substitution r)
	      (when (and p other)
		(remove-parameter% p s)
		(push p (slot-value s 'eliminated-parameters))
		(add-relations (create-composite-relation
				(remove-if-not #'(lambda (r) (find r (relations s)))
						      (relations p))
				p r other)
			       s)
		(remove-relations% (relations p) s))))))


(defun create-evaluator (relation parameters)
  (let* ((params (slot-value relation 'parameters))
	 (pos (loop for param in params
		 collect (position param parameters))))
    (lambda (x)
      (loop for i in pos
	 for p in params do
	   (when i (setf (value p) (grid:aref x i))))
      (eval-relation relation))))	 

(defun solve-system-levenberg (relations parameters)
  (when (and relations parameters)
	(if (< (length relations) (length parameters))
		(error "there are fewer relations ~a than parameters ~a" relations parameters))
    (let ((soln
		   (levenberg-solve
			;; list of functions to solve
			(loop for r in relations collect (create-evaluator r parameters))
			;; intial value 
			(loop for p in parameters collect
				 (if (and (slot-boundp p 'value) (value p))
				     (value p)
				     (setf (value p) (1+ (random 10))))))))
      (loop for p in parameters
		 for i from 0 do
		   (setf (value p) (grid:aref soln i))))))

(defmethod add-solution-finalizer ((func function) (s system))
  (when (slot-value s 'solution-finalizer)
	(error "Solution-Finalizer already exists for the system ~a" s))
  (setf (slot-value s 'solution-finalizer) func))

(defun solve-system (&optional (system *system*))
  (assert (typep system 'system))
  (simple-solve system)
  (dimensionally-reduce system)
  (solve-system-levenberg (relations system) (parameters system))
  (anaphora:awhen (unknowns (slot-value system 'eliminated-parameters))
    (loop for p in anaphora:it do
		 (simple-solve-parameter p)))
  ;; TODO/TO-THINK: Maybe before running finalizer the solved parameters must be set to solved
  ;; i.e. no longer considered variables
  ;; THOUGHT : lets not do that. The finalizer may add or remove relations relating the already solved
  ;; parameters according to the values they attain. 
  (let ((finalizer (slot-value system 'solution-finalizer)))
	(when finalizer
	  (assert (typep finalizer 'function))
	  (setf (slot-value system 'solution-finalizer) nil)
	  (funcall finalizer system)))
  system)

(defun solve-for (param/s)
  "Search for relations and find consistent solution for solvable parameters"
  (print "DEPRECATED NAME use (make-system param/s :then :solve)")
  (solve-system (find-solvable-system-for (alexandria:ensure-list param/s) :largest)))

(defun make-system (parameters &key then)
  (let ((system (find-solvable-system-for (alexandria:ensure-list parameters) :largest)))
	(cond ((eql then :solve)
		   (solve-system system))
		  ((eql then nil)
		   system)
		  (t
		   (error ":then :solve or :then nil")))))
