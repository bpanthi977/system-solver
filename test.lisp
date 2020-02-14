(defpackage :system-solver-test
  (:use :cl :hydraulics :system-solver))
(in-package :system-solver-test)

(defmacro solve-values (&rest parameters)
  (alexandria:with-gensyms ((params "params"))
	`(let ((,params (list ,@parameters)))
	   (solve-for ,params)
	   (loop for p in ,params
		  collect (value p)))))

(defun nearly-same (values1 values2 &optional (tolerance 0.00001))
  (loop for v1 in values1
	 for v2 in values2 do
	   (unless (< (abs (- v1 v2)) tolerance)
		 (format t  "Values not same ~a <> ~a~%" values1 values2)
		 (return nil))
	 finally (progn
			   (print "test ok")
			   (return t))))	   

(defmacro test-nearly (values tolerance &body body)
  (alexandria:with-gensyms ((bodysym "body"))
	(alexandria:once-only (values tolerance)
	  `(let ((,bodysym (progn ,@body)))
		 (nearly-same ,values ,bodysym (if (and (typep t 'boolean)
												(eq ,tolerance t))
										   0.0001
										   ,tolerance))))))

;; Some random set of functions
(define-component test
    :parameters (a b c)
    :relations ((lambda (a b c) (+ (expt a 3) (expt b 3/2) (expt c 0.2) -7.51532))
				(lambda (a b c) (+ (expt a 2) (* 3 a (/ b) c) -5.832))
				(lambda (a b c) (+ (expt a 4/3) (* c (log b) (/ (log 10))) (* a b) -5.9663))))

(test-nearly (list 1.229968 2.684053 3.141789) t
	(let ((dtest (make-instance 'test)))
	  (solve-values (slot-value dtest 'a)
					(slot-value dtest 'b)
					(slot-value dtest 'c))))

(test-nearly (list 3 -2) T	
  (with-parameters ((a 3)
					b)
    (satisfying-relations (lambda (a b) (+ a b -2))
						  (lambda (a b) (- a b 5)))
    (solve-values a b)))


;;;; TEST
;; (defun test-solve1 ()
;;   (flet ((xi (x i) (grid:aref x i)))
;;     (levenberg-solve (list (lambda (x)
;; 							 (+ (* (expt (xi x 0) 2) (xi x 1)) -2))
;; 						   (lambda (x)
;; 							 (+ (xi x 0) (- (xi x 1)) -4))
;; 						   (lambda (x)
;; 							 (+ (xi x 0) (xi x 1) -5.20d0)))
;; 					 (list 0.0d0 1.0d0))))


;; (defun residue (fs xs)
;;   (setf xs (grid:make-foreign-array 'double-float :initial-contents xs))
;;   (loop for f in fs
;;      collect (realpart (funcall f xs))))

;; (defun solve-system-saved (sys)
;;   (solve-system4 (getf sys :functions)
;; 				 (getf sys :guess)))

;;;;; Further tests

(test-nearly (list 0.59104235) t
	(let ((p (make-instance 'pipe :D 0.8 :e 1.6d-3 :q 5 :l 100 :nu 1d-6)))
	  (solve-values (slot-value p 'r))))


(test-nearly (list 3.1415946) t
  (let* ((p1 (make-instance 'pipe :vel 1 :d 2 :nu 0.03))
		 (unknown (slot-value p1 'Q)))
    (solve-values  unknown)))


(test-nearly (list 10.0 10.0) t 
  "Pressure formula check"
  (let* ((p1 (make-instance 'pipe :name "p1" :p2 10))
		 (p2 (make-instance 'pipe :name "p2"))
		 (p3 (make-instance 'pipe :name "p3")))
    (connect-pipes (list p1 p2 p3) (list t t t))
    (solve-values (slot-value p2 'p2)
				  (slot-value p3 'p2))))

(test-nearly (list -15) t 
  "Continuity check"
  (let* ((p1 (make-instance 'pipe :name "p1" :q 10))
		 (p2 (make-instance 'pipe :name "p2" :q 5))
		 (p3 (make-instance 'pipe :name "p3")))
    (connect-pipes (list p1 p2 p3) (list t t t))
	(solve-values 
				(slot-value p3 'q))))

(test-nearly (list 45.0) t 
  "Head loss and head loss 2 check"
  (let* ((p1 (make-instance 'pipe :name "p1" :p1 2 :p2 8))
		 (p2 (make-instance 'pipe :name "p2" :r 5 :Q 3))
		 (p3 (make-instance 'pipe :name "p3")))
    (connect-pipes (list p1 p2 p3) (list t t t))
    (solve-values (slot-value p2 'hf))))

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

(screamer::defun find-loops0 (starting-node node adjacent-nodes-fn &optional visited-nodes)
  (let* ((connected-nodes  (set-difference (funcall adjacent-nodes-fn node) visited-nodes))
		 (next-node (screamer:a-member-of connected-nodes)))
    (if (eql next-node starting-node)
		(list next-node)
		(cons next-node (find-loops0 starting-node next-node adjacent-nodes-fn (cons next-node visited-nodes))))))

(defun find-loops (network adjacent-nodes-fn)
  (let* ((visited nil)
		 (loops nil)
		 (all-loops (screamer:all-values
					  (let ((node (screamer:a-member-of network)))
						(setf loops (find-loops0 node node adjacent-nodes-fn visited))
						(screamer:global (pushnew node visited))
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
  (let ((discharge-relation (make-instance 'hydraulics::continuity))
		(node (make-instance 'node :pipes pipes :ends ends :name name :number (length *pipe-network-nodes*))))
    (push node *pipe-network-nodes*)
    (connect-with-previous-nodes node)
    (loop for p in pipes
       for endp in ends do 
		 (hydraulics::add-discharge-connection (slot-value p 'q) discharge-relation endp))))

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

(test-nearly (list 43.85435815573788 58.5189154603014 2.3732736160392625) t 
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
    (solve-values (slot-value cd 'q)
				  (slot-value ab 'q)
				  (slot-value bc 'q))))

;;WARNING: Cannot reach the specified tolerance in F; in ITERATE 
;; (defun t5 ()
;;   (let* ((1a (make-instance 'pipe :name "1a" :r 0 :q 5.0))
;; 		 (ab (make-instance 'pipe :name "ab" :r 2))
;; 		 (b2 (make-instance 'pipe :name "b2" :r 0 :q 2.5))
;; 		 (bc (make-instance 'pipe :name "bc" :r .5))
;; 		 (3c (make-instance 'pipe :name "3c" :q 2))
;; 		 (cd (make-instance 'pipe :name "cd" :r 1.5))
;; 		 (d4 (make-instance 'pipe :name "d4" :q 3))
;; 		 (de (make-instance 'pipe :name "de" :r .5))
;; 		 (5e (make-instance 'pipe :name "5e" :q 5))
;; 		 (ef (make-instance 'pipe :name "ef" :r .5))
;; 		 (f6 (make-instance 'pipe :name "f6" :q 2.5))
;; 		 (af (make-instance 'pipe :name "af" :r 2))
;; 		 (ad (make-instance 'pipe :name "ad" :r 200))
;; 		 (*pipe-network-nodes* nil))
;;     (add-node (list 1a ab ad af) (list t nil nil nil) "a") ;a
;;     (add-node (list ab bc b2) (list t nil nil) "b") ;b 
;;     (add-node (list bc cd 3c) (list t nil t) "c") ;c
;;     (add-node (list d4 ad de cd) (list nil t nil nil) "d") ;d
;;     (add-node (list 5e de ef) (list t t nil) "e");e
;;     (add-node (list ef af f6) (list t t nil) "f");f

;;     ;; (loop for node in *pipe-network-nodes* do
;;     ;; 	 (print node)
;;     ;; 	 (print (slot-value node 'links)))
;;     (add-pipe-network-loop-equations)
;;     (solve-for (slot-value ad 'q))))



(test-nearly '(3.0 -1.0) t
  (Define-relation r1
	  :parameters (x y)
	  :implicit (+ x y -2))
  (define-relation r2
	  :parameters (x y)
	  :implicit (+ x (- y) -4))
  (let ((x (make-instance 'parameter :name "x"))
		(y (make-instance 'parameter :name "y")))
	(make-instance 'r1 :x x :y y)
	(make-instance 'r2 :x x :y y)
	(solve-values x y)))



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


;; (three-reservoir-problem)
;; (defparameter 3-sys *last-system*)
;; (test-dimensional-reduce)
;; (defparameter t-sys *last-system*)
;; (setf (getf 3-sys :guess) (reverse (getf t-sys :guess)))
;; (solve-system-saved 3-sys)
;; (solve-system-saved t-sys)
(test-nearly (list 1 -1) t 
  (with-parameters ((a 1) b)
	(satisfying-relations (lambda (a b)
							(if (and (> a  -2) (< a 2))
								(+ a b)
								(if (> a 5)
									(- a b)))))
	(solve-values a b)))
	

