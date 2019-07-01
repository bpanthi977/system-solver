;;;; system-solver.lisp
(in-package #:system-solver)

;; System Class 
(defclass system ()
  ((parameters :accessor parameters :initarg :parameters)
   (relations :accessor relations :initarg :relations)
   (solved-parameters :accessor solved-parameters :initform nil)
   (removed-parameters :initform nil)))

(defmethod initialize-instance :after ((s system) &key)
  (with-slots (parameters) s 
    (let ((unknowns (remove-if #'known-parameter-p parameters))
	  (knowns (remove-if-not #'known-parameter-p parameters)))
      (setf (solved-parameters s) knowns
	    parameters unknowns))))

(defmethod remove-parameter ((p parameter) (s system))
  (with-slots (parameters solved-parameters removed-parameters) s
    (setf parameters (remove p parameters))
    (if (known-parameter-p p)
	(push p solved-parameters)
	(push p removed-parameters))))

(defmethod remove-relations ((rs list) (s system))
  (setf (relations s) (set-difference (relations s) rs)))

(defmethod add-relations ((rs list) (s system))
  (setf (relations s) (union (relations s) rs)))

(defmethod print-object ((s system) stream)
  (with-slots (parameters solved-parameters removed-parameters relations) s 
    (format stream "~a+~a parameters ~a relations"
	    (length parameters) (length removed-parameters) (length relations))
    (loop for p in parameters do
	 (print p stream))
    (loop for p in solved-parameters do
	 (print p stream))
    (loop for p in removed-parameters do
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
      (make-instance 'system :relations relations :parameters parameters))))

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

(defmethod simple-solve ((s system))
  "Solve a system as far as possible using one-to-one relations, ie without using simultaneous relations"
  (loop for p in (parameters s)
     with solved = 0 do 
       (unless (known-parameter-p p) 
	 (loop for r in (relations p)
	    for us = (unknowns r)
	    when (= (length us) 1)
	    do (progn (solve-relation p r)
		      (remove-parameter p s)
		      (remove-relations (list r) s)
		      (incf solved)
		      (return))))
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
		(remove-parameter p s)
		(add-relations (create-composite-relation
				(remove-if-not #'(lambda (r) (find r (relations s)))
						      (relations p))
				p r other)
			       s)
		(remove-relations (relations p) s))))))


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
    (let ((soln
	   (levenberg-solve (loop for r in relations collect (create-evaluator r parameters))
			    (loop for p in parameters collect
				 (if (slot-boundp p 'value)
				     (value p)
				     (setf (value p) (1+ (random 10))))))))
      (loop for p in parameters
	 for i from 0 do
	   (setf (value p) (grid:aref soln i))))))

(defun solve-system (system)
  (simple-solve system)
  (dimensionally-reduce system)
  (solve-system-levenberg (relations system) (parameters system))
  system)

(defun solve-for (param/s)
  "Search for relations and find consistent solution for solvable parameters"
  (solve-system (find-solvable-system-for (alexandria:ensure-list param/s) :largest)))
