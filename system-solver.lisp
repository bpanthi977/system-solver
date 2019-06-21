;;;; system-solver.lisp

(in-package #:system-solver)
(setf *read-default-float-format* 'double-float)

(defclass parameter ()
  ((name :initarg :name)
   (value :initarg :value :accessor value)
   (relations :initarg :relations :accessor relations :initform nil)
   (dependencies :initarg :dependencies :accessor dependencies)))

(defmethod print-object ((p parameter) s)
  (format s "<#P ~a>" (slot-value p 'name)))

(defclass relation ()
  ((parameters :initarg :params :accessor parameters :initform nil)
   (parameter-slots :initarg :parameter-slots :initform nil)
   (implicit :initarg :implicit)
   (unsolvable-parameters :initarg :unsolvable-parameters :initform nil)
   (multiplicity :initarg multiplicity :initform 1)
   (name :initarg :name :type 'string :initform "")))

(defmethod print-object ((r relation) s)
  (format s "<#~a ~a" (slot-value r 'name) (slot-value r 'parameters)))

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

(defun newton-solve (g g-prime &optional (x0 1) (tolerance 0.0001) (max-iterations 50))
  "Solve for root of g(x) = 0 within tolerance;
 given derivative of g (g-prime) and initial guess x0"
  (let ((g0 (funcall g x0))
	g-prime0)	
    (loop for i from 0 to max-iterations do
	 (when (< (abs g0) tolerance)
	   (return (values x0 tolerance i)))

	 (setf g-prime0 (funcall g-prime x0))
	 (setf x0 (- x0 (/ g0 g-prime0)))
	 (setf g0 (funcall g x0))
	 finally (return (values x0 g0 max-iterations)))))


(defun newton-solver-general (g &optional (x0 1) (tolerance 0.00001) (max-iterations 50) (step  0.01))
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
  (when (slot-boundp c 'name)
    (update-parameter-names c)))

(defmethod initialize-instance :after ((r relation) &key)
  (convert-slot-values-to-parameters (slot-value r 'parameter-slots) r)
  (setf (slot-value r 'parameters) (loop for v in (slot-value r 'parameter-slots)
				      for p = (slot-value r v) do
					(unless (find v (slot-value r 'unsolvable-parameters))
					  (pushnew r (slot-value p 'relations) :test #'eql))
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
		  (apply implicit (concatenate 'list pre (list x) post)))))
	(setf (value (slot-value r var)) (newton-solver-general g 0))))))

(defmethod solve-relation ((p parameter) (r relation))
  (solve-relation (nth (position p (slot-value r 'parameters))
		       (slot-value r 'parameter-slots))
		  r))

(defmethod eval-relation ((r relation))
  (if (slot-boundp r 'implicit)
      (let ((params (mapcar #'(lambda (slot) (value (slot-value r slot))) ;; TODO: sorting can be done beforehand
			    (slot-value r 'parameter-slots))))
	(apply (slot-value r 'implicit) params))
      (error "Cannot evaluate relation ~a" r)))

(defun known-parameter-p (parameter)
  "Is the parameter's value known?"
  (and (slot-boundp parameter 'value)
       (value parameter)))
  

(defun eval-functions (functions)
  (map 'vector #'eval-relation functions))

(defun norm (vector)
  (sqrt (iterate:iter (iterate:for i in-vector vector)
		      (iterate:summing (expt i 2)))))

(defun norm2 (vector)
  (loop for i across vector
       summing i))

(defun solve-system0 (functions parameters initial &optional (increment 0.1))
  (loop
     for x in parameters
     for x0 = (value x)
     for x1 = (incf (value x) increment) 
     for new = (eval-functions functions)
     for dv = (map 'vector (lambda (a b) (* (signum a) (- b a)))
		   initial new)
     for norm2-dv = (norm2 dv)
     for new-x = (if (= 0 norm2-dv) x1 (- x0  (* (norm initial) increment (/ norm2-dv))))
     with new-values = nil do
       ;; (print (list x0 x1 new dv norm2-dv new-x))
       (setf (value x) x0
	     initial (eval-functions functions))
       ;; (print new-values)
       (push new-x new-values)
     finally (progn
	       (loop for p in parameters
		  for v in (reverse new-values) do
		    (setf (value p) v))
	       (return initial))))

(defun solve-system (functions parameters &optional (max-iterations 10) (tolerance 0.1))
  (loop for p in parameters do
       (unless (slot-boundp p 'value)
	 (setf (value p) 1)))
  (loop for x from 1 to max-iterations
     for v_n = (eval-functions functions)
     for v_n+1 = (solve-system0 functions parameters v_n) do
       ;; (print-parameters parameters)
       (print v_n+1)
       (print (norm v_n+1))
       (if (< (norm v_n+1) tolerance)
	   (return t))
     finally
       (progn
	 (print "Wasn't within tolerance"))))

(defmethod unknowns ((r relation))
  (remove-if #'known-parameter-p (slot-value r 'parameters)))

(defmethod unknowns ((l list))
  (remove-if #'known-parameter-p l))

(defun solve-parameter (p &key traversed-relations traversed-parameters)
  ;; (print "solving parameter")
  (when (or (known-parameter-p p) (find p traversed-parameters))
    (return-from solve-parameter nil))
  (let* ((relation (a-member-of (relations p)))
	 unknowns)
    ;; (format t "~%~a ~% tr: ~a ~% tp: " (slot-value p 'name) traversed-relations)
    (loop for p in traversed-parameters do (format t "~a, " (slot-value p 'name)))
    ;; (print nil)
    (when (find relation traversed-relations)
      ;; (format t "already ~a" relation)
      (fail))
    ;; (print (slot-value relation 'name))
    ;; (print-parameters (slot-value relation 'parameters))
    ;; (inspect relation)
    (let ((traversed-relations (cons relation traversed-relations))
	  (traversed-parameters (cons p traversed-parameters)))
      (setf unknowns (unknowns relation))
      (if unknowns
	  (progn
	    (loop for unknown in unknowns do
		 (unless (find unknown traversed-parameters)
		     (progn 
		       ;; (format t "~% solving for ~a" (slot-value unknown 'name))
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
       ;; (print "Parameter")
       ;; (print (slot-value p 'name))
     ;; (inspect r)
       ;; (print-relation-parameters r)
       (solve-relation p r)
       (format t "~%~a = ~a" (slot-value p 'name) (value p))))

(defun parameter-values (parameters)
  (loop for p in parameters collect (value p)))

(defun execute-solution-strategy (unknown relations parameters
				  &optional (initial-guess 1) (tolerance 0.0001))
  (unless relations
    (print "No strategy to execute")
    (return-from execute-solution-strategy (value unknown)))
  (loop for p in parameters do       
       (unless (slot-boundp p 'value)
	 (setf (slot-value p 'value) 0)))
  (setf (value unknown) initial-guess)
  (loop for count from 0 to 10
     for previous-values = (parameter-values parameters)
     for unknown-prev-value = (value unknown)
     with params-stable = nil do 
       (execute-solution-strategy0 relations parameters)
       ;; (print unknown-prev-value)
       (when (< (abs (- unknown-prev-value (value unknown))) tolerance)
	 (loop for p in parameters
	    for vp in previous-values do 
	      (unless (< (abs (- vp (value p))) tolerance)
		(format t "Not within tolerance ~a" (slot-value p 'name))
		(return))
	    finally (setf params-stable t))
	 (when params-stable
	   (return (value unknown)))
	 (setf params-stable nil))
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

(defun solve-for0 (unknown/s)
  (one-value
   (let (relations parameters unknown)
     (loop for u in (alexandria:ensure-list unknown/s) do
	  (setf unknown u)
	  (multiple-value-bind (r p) (solve-parameter unknown
						      :traversed-parameters parameters
						      :traversed-relations relations)
	    (setf relations (append r relations))
	    (setf parameters (append p parameters))))
     (print-parameters parameters)
     (print relations)
     (print "Executing strategy")
     (execute-solution-strategy unknown relations parameters)
     (print-parameters parameters)
     (print (value unknown)))))

(defun solve-for1 (parameters relations)
  (solve-system5 relations parameters))


(defun solve-for2 (unknown/s)
  (let (relations parameters unknown)
    (loop for u in (alexandria:ensure-list unknown/s) do
	 (setf unknown u)
	 (all-values (multiple-value-bind (r p) (solve-parameter unknown
								 :traversed-parameters parameters
								 :traversed-relations relations)
		       (setf relations (append r relations))
		       (setf parameters (append p parameters)))))
    (setf relations (remove-duplicates relations))
    (setf parameters (remove-duplicates parameters))
    (print relations)
    (print-parameters parameters)
    (print "Solving")
    (solve-for1  parameters relations)
    (print-parameters parameters)))

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

(defmethod simple-solve ((s system))
  (loop for p in (parameters s) do 
     (unless (known-parameter-p p) 
       (loop for r in (relations p)
	  for us = (unknowns r)
	  when (= (length us) 1)
	  do (progn (solve-relation p r)
		    (remove-parameter p s)
		    (return))))))

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
    (format s "<# CR ep:~a sp:~a (~a)";; or:~a ep-r:~a" ep sp originalr ep-r)))
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
  
(defmethod compose-relations ((ep parameter) (es-r relation) (sp parameter))
  "Replace all relations of ep (eliminated parameter) by composing new relations wrt sp (substituted parameter) using es-r; 
Thus n(unknown_params(r)) = 2"
  (let* ((p_0 nil)
	 (o_0 nil)
	 (p->o (lambda (p)
		 (if (and p_0
			  (= p p_0))
		     o_0
		     (setf p_0 p
			   o_0 (solve-relation sp es-r)))))
	 (o->p (lambda (o)
		 (if (and o_0
			  (= o o_0))
		     p_0
		     (setf o_0 o
			   p_0 (solve-relation ep es-r)))))
	 cr crs)
    (loop for r in (relations ep) do
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

(defmethod dimensionally-reduce ((s system))
  (loop for p in (parameters s) do
       (loop for r in (remove-if-not #'(lambda (r)
					 (= (length (unknowns r)) 2))
				  (relations p))
	  when (find p (parameters s)) do
	    ;; (print (unknowns r))
		     (multiple-value-bind (p other)
			 (choose-for-substitution r)
		       (when (and p other)
			 (print (list p other r))
			 (remove-parameter p s)
			 (remove-relations (relations p) s)
			 (add-relations (compose-relations p r other) s))))))
		       
(defun solve-for (params)
  (multiple-value-bind (rs ps) (search-params/rels params)
    (let* ((system (make-instance 'system :parameters ps :relations rs))
	   (simple-solved (simple-solve system)))
      (dimensionally-reduce system)
      
      (solve-for2 params)
      ;; (inspect system))))
      system)))


;;;; TESTING

;; dimensionally-reduce test
(define-relation r1
    :parameters (a b)
    :implicit (- a (* 2 b)))
(define-relation r2
    :parameters (b c)
    :implicit (- b (expt c 2) -10))
(define-relation r3
    :parameters (a c)
    :implicit (- a  c 5))

(define-component dtest
      :parameters (a b c)
      :relations ((:relation r1 :parameters (:a a :b b))
  		  (:relation r2 :parameters (:b b :c c))
  		  (:relation r3 :parameters (:a a :c c))))

(defun test-dimensional-reduce ()
  (let ((dtest (make-instance 'dtest)))
    (solve-for (list (slot-value dtest 'a)))))

(define-component pipe
    ;; :parameters (Re vel D nu Q A r hf p1 p2)
    :parameters (Q r hf p1 p2)
    :relations (;; (:relation pipe-discharge
		;; 	   :parameters (:Q q :v vel :d d))
		;; (:relation reynolds-number
		;; 	   :parameters (:Re Re :v vel :nu nu :D D))
		(:relation head-loss
			   :parameters (:hf hf :r r :q q))
		(:relation head-loss-2
			   :parameters (:hf hf :p1 p1 :p2 p2))))

(defun tt ()
  (let* ((p1 (make-instance 'pipe :vel 1 :d 2 :nu 0.03))
	 (unknown (slot-value p1 'Q)))
    (solve-for (list unknown))))

(defun tt2 ()
  (let* ((p1 (make-instance 'pipe :vel 1 :d 2 :nu 0.03))
	 (unknown (slot-value p1 'Q)))
    (solve-for2 unknown)))
    

(defun three-reservoir-problem ()
  (let* ((p1 (make-instance 'pipe :name "p1" :r 15938 :p1 24))
	 (p2 (make-instance 'pipe :name "p2" :r 83565 :p1 8))
	 (p3 (make-instance 'pipe :name "p3" :r 170014 :p1 0))
	 (unknown (slot-value p1 'q)))
    (connect-pipes (list p1 p2 p3) (list t t t))
    (solve-for2 unknown)))

(defun three-reservoir-problem2 ()
  (let* ((p1 (make-instance 'pipe :name "p1" :r 2.52 :p1 60))
	 (p2 (make-instance 'pipe :name "p2" :r 16.26 :q 0.3))
	 (p3 (make-instance 'pipe :name "p3" :r 17.98 :p1 38))
	 (unknown (slot-value p3 'q)))
    (connect-pipes (list p1 p2 p3) (list t t t))
    (solve-for2 (list unknown (slot-value p2 'p1)))))

(defun tt3 ()
  "Pressure formula check"
  (let* ((p1 (make-instance 'pipe :name "p1" :p2 10))
	 (p2 (make-instance 'pipe :name "p2"))
	 (p3 (make-instance 'pipe :name "p3")))
    (connect-pipes (list p1 p2 p3) (list t t t))
    (solve-for2 (list (slot-value p2 'p2)
		      (slot-value p3 'p2)))))

(defun tt4 ()
  "Continuity check"
  (let* ((p1 (make-instance 'pipe :name "p1" :q 10))
	 (p2 (make-instance 'pipe :name "p2" :q 5))
	 (p3 (make-instance 'pipe :name "p3")))
    (connect-pipes (list p1 p2 p3) (list t t t))
    (solve-for2 (list
		      (slot-value p3 'q)))))

(defun tt5 ()
  "Head loss and head loss 2 check"
  (let* ((p1 (make-instance 'pipe :name "p1" :p1 2 :p2 8))
	 (p2 (make-instance 'pipe :name "p2" :r 5 :Q 3))
	 (p3 (make-instance 'pipe :name "p3")))
    (connect-pipes (list p1 p2 p3) (list t t t))
    (solve-for2 (list (slot-value p1 'hf)
		      (slot-value p2 'hf)))))

(defun connect-pipes (pipes ends)
  (let ((discharge-relation (make-instance 'continuity)))
    (loop for p in pipes
       for endp in ends
       with prev-pipe-p = nil do 
	 (add-discharge-connection (slot-value p 'q) discharge-relation endp)
	 (when prev-pipe-p
	   (make-instance 'equal-pressure :p1 prev-pipe-p :p2 (slot-value p (if endp 'p2 'p1))))
;;	   (setf (slot-value p (if endp 'p2 'p1)) prev-pipe-p)
	 (setf prev-pipe-p (slot-value p (if endp 'p2 'p1))))))
	   

(defun t4 ()
  (let* ((1a (make-instance 'pipe :name "1A" :p1 0 :r 0 :q 100))
	 (ab (make-instance 'pipe :name "AB" :r 1))
	 (b2 (make-instance 'pipe :name "B2" :r 0 :q 25))
	 (bc (make-instance 'pipe :name "BC" :r 3))
	 (bd (make-instance 'pipe :name "BD" :r 2))
	 (ac (make-instance 'pipe :name "AC" :r 2))
	 (cd (make-instance 'pipe :name "CD" :r 1))
	 (d3 (make-instance 'pipe :name "D3" :r 0 :q 75)))
    (connect-pipes (list 1a ab ac) '(t nil nil))
    (connect-pipes (list ab bd bc b2) '(t nil nil nil))
    (connect-pipes (list bd cd d3) '(t t nil))
    (connect-pipes (list ac bc cd) '(t t nil))
    (solve-for2 (list (slot-value cd 'q)
		     (slot-value bc 'hf)))))

;;
(defparameter *pipe-network-nodes* nil)
(defclass node ()
  ((links :initarg :links :initform nil)
   (pipes :initarg :pipes :initform nil)
   (ends :initarg :ends :initform nil)
   (name :initarg :name :initform nil)
   (number :initarg :number)))

(defmethod print-object ((n node) s)
  (format s "#<Node ~a>" (slot-value n 'name)))

(defun find-loops0 (starting-node node adjacent-nodes-fn &optional visited-nodes)
  (let* ((connected-nodes  (set-difference (funcall adjacent-nodes-fn node) visited-nodes))
	 (next-node (screamer:a-member-of connected-nodes)))
    (if (eql next-node starting-node)
	(list next-node)
	(cons next-node (find-loops0 starting-node next-node adjacent-nodes-fn (cons next-node visited-nodes))))))

(defun find-loops (network adjacent-nodes-fn)
  (let* ((visited nil)
	(loops nil)
	 (all-loops (all-values
		     (let ((node (a-member-of network)))
		       (setf loops (find-loops0 node node adjacent-nodes-fn visited))
		       (global (pushnew node visited))
		       loops))))
    (remove-if #'(lambda (l) (< (length l) 3))
	       all-loops)))

(defun connect-with-previous-nodes (node)
  (loop for n in *pipe-network-nodes*
     with mypipes = (slot-value node 'pipes) do
       (when (not (eql n node))
	 (loop for p in (slot-value n 'pipes) do 
	      (when (find p mypipes)
		(pushnew n (slot-value node 'links))
		(pushnew node (slot-value n 'links))
		(return))))))

(defun add-node (pipes ends &optional name)
  (let ((discharge-relation (make-instance 'continuity))
	(node (make-instance 'node :pipes pipes :ends ends :name name :number (length *pipe-network-nodes*))))
    (push node *pipe-network-nodes*)
    (connect-with-previous-nodes node)
    (loop for p in pipes
       for endp in ends do 
	 (add-discharge-connection (slot-value p 'q) discharge-relation endp))))

(defun loop-pipes (nodes)
  (loop for n in nodes
     with prev = (first (last nodes))
     with pipes = nil
     with signs = nil
     for pipe = (first (intersection (slot-value n 'pipes) (slot-value prev 'pipes)))
     for end =  (nth (position pipe (slot-value prev 'pipes))
		     (slot-value prev 'ends)) do
       (push pipe pipes)
       (push (if end 1 -1) signs)
       (setf prev n)
     finally (return (list pipes signs))))
       

(defun add-pipe-network-loop-equations ()
  (let ((loops (remove-duplicates
		(find-loops *pipe-network-nodes*
				   (lambda (node)
				     (slot-value node 'links)))
		:test #'equal
		:key #'(lambda (l) (sort (loop for n in l collect (slot-value n 'number)) #'<)))))
    ;; (print loops)
    (loop for l in loops
       for (pipes signs) = (loop-pipes l)
       for eq = (make-instance 'loop-head-loss :pipes pipes :signs signs))))

(defclass loop-head-loss (relation)
  ((pipes :initarg :pipes)
   (name :initform "Σ head loss in a loop = 0" :allocation :class)
   (rs :initform nil :initarg :rs)
   (qs :initform nil :initarg :qs)
   (signs :initform nil :initarg :signs)))

(defmethod initialize-instance :after ((i loop-head-loss) &key)
  (with-slots (qs rs parameters) i
    (loop for pipe in (reverse (slot-value i 'pipes))
       for q = (slot-value pipe 'q)
       for r = (slot-value pipe 'r) do
	 (push i (slot-value q 'relations))
	 (push i (slot-value r 'relations))
	 (push q qs)
	 (push r rs)
	 (push q parameters)
	 (push r parameters))))

(defun signed-sqrt (n)
    (if (< n 0)
	(- (sqrt (- n)))
	(sqrt n)))

(defmethod solve-relation ((var parameter) (relation loop-head-loss))
  (loop for q in (slot-value relation 'qs)
     for r in (slot-value relation 'rs)
     for s in (slot-value relation 'signs)
     with vq = nil
     with vr = nil
     with vs = nil
     with vtype = nil
     with sum = 0 do
       (cond ((eql var r)
	      (setf vq q vr r vs s vtype :r))
	     ((eql var q)
	      (setf vq q vr r vs s vtype :q))
	     (t (incf sum (* s (signum (value q)) (value r) (expt (value q) 2)))))
     finally (ecase vtype
	       (:r (setf (value vr) (abs (/ sum (expt (value vq) 2)))))
	       (:q (setf (value vq) (signed-sqrt (/ (- sum) vs (value vr))))))))
       
(defmethod eval-relation ((rel loop-head-loss))
  (loop for q in (slot-value rel 'qs)
     for r in (slot-value rel 'rs)
     for s in (slot-value rel 'signs)
     summing (* s (signum (value q)) (value r) (expt (value q) 2))))

(defun t4 ()
  (let* ((1a (make-instance 'pipe :name "1A" :r 0 :q 100))
	 (ab (make-instance 'pipe :name "AB" :r 1))
	 (b2 (make-instance 'pipe :name "B2" :r 0 :q 25))
	 (bc (make-instance 'pipe :name "BC" :r 3))
	 (bd (make-instance 'pipe :name "BD" :r 2))
	 (ac (make-instance 'pipe :name "AC" :r 2))
	 (cd (make-instance 'pipe :name "CD" :r 1))
	 (d3 (make-instance 'pipe :name "D3" :r 0 :q 75))
	 (*pipe-network-nodes* nil))
    (add-node (list 1a ab ac) '(t nil nil) "a")
    (add-node (list ab bd bc b2) '(t nil nil nil) "b")
    (add-node (list bd cd d3) '(t t nil) "d")
    (add-node (list ac bc cd) '(t t nil) "c")
    (add-pipe-network-loop-equations)
    (solve-for2 (slot-value cd 'q))))

(defun t5 ()
  (let* ((1a (make-instance 'pipe :name "1a" :r 0 :q 5.0))
	 (ab (make-instance 'pipe :name "ab" :r 2))
	 (b2 (make-instance 'pipe :name "b2" :r 0 :q 2.5))
	 (bc (make-instance 'pipe :name "bc" :r .5))
	 (3c (make-instance 'pipe :name "3c" :q 2))
	 (cd (make-instance 'pipe :name "cd" :r 1.5))
	 (d4 (make-instance 'pipe :name "d4" :q 3))
	 (de (make-instance 'pipe :name "de" :r .5))
	 (5e (make-instance 'pipe :name "5e" :q 5))
	 (ef (make-instance 'pipe :name "ef" :r .5))
	 (f6 (make-instance 'pipe :name "f6" :q 2.5))
	 (af (make-instance 'pipe :name "af" :r 2))
	 (ad (make-instance 'pipe :name "ad" :r 200))
	 (*pipe-network-nodes* nil))
    (add-node (list 1a ab ad af) (list t nil nil nil) "a") ;a
    (add-node (list ab bc b2) (list t nil nil) "b") ;b 
    (add-node (list bc cd 3c) (list t nil t) "c") ;c
    (add-node (list d4 ad de cd) (list nil t nil nil) "d") ;d
    (add-node (list 5e de ef) (list t t nil) "e");e
    (add-node (list ef af f6) (list t t nil) "f");f

    ;; (loop for node in *pipe-network-nodes* do
    ;; 	 (print node)
    ;; 	 (print (slot-value node 'links)))
    (add-pipe-network-loop-equations)
     (solve-for2 (slot-value ad 'q))))

    

(defun t5 ()
  (define-relation r1
      :parameters (x y)
      :implicit (+ x y -2))
  (define-relation r2
      :parameters (x y)
      :implicit (+ x (- y) -4))
  (let ((x (make-instance 'parameter :name "x"))
	(y (make-instance 'parameter :name "y")))
    (make-instance 'r1 :x x :y y)
    (make-instance 'r2 :x x :y y)
    (solve-for2 (list x y))))
  

(define-relation reynolds-number
    :parameters (Re v nu D)
    :implicit (- Re (* v D (/ nu)))
    :name "Definition of Reynolds Number")

(define-relation pipe-discharge 
    :name "Discharge through pipe"
    :parameters (Q v d)
    :implicit (- Q (* v (/ pi 4) (expt d 2))))

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
    :implicit (+ hf (- p1) p2)
    :name "Head loss depending on pressure difference")

(defclass continuity (relation)
  ((name :initform "Continuity at a junction" :allocation :class)
   (parameters :initform nil)
   (parameter-slots :initform nil)
   (factors :initform nil)))

(defmethod add-discharge-connection ((p parameter) (c continuity) &optional (end t))
  (with-slots (parameters parameter-slots factors) c
    (push p parameters)
    (push (if end 1 -1) factors)
    (push c (relations p))
    (let ((f (first parameter-slots)))
      (push (if f (1+ f) 1) parameter-slots))))

(defmethod solve-relation ((var number) (r continuity))
  (with-slots (parameters factors) r
    (loop for p in parameters
       for f in factors
       with req = (nth (1- var) (reverse parameters))
       with req-f = (nth (1- var) (reverse factors))
       with sum = 0 do
	 (unless (eql p req)
	   (incf sum (* f (value p))))
       finally
	 (setf (value req) (* req-f (- sum))))))

(defmethod eval-relation ((r continuity))
  (with-slots (parameters factors) r
    (* 1 (loop for p in parameters
	      for f in factors
	      summing (* f (value p))))))

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
