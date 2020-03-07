(in-package :system-solver)

(defclass table2D (relation-managed)
  ((data :type :list :initarg :data :initform nil)
   (interpolation :initform '(t t) :initarg :interpolation)
   (index1 :initform nil :initarg :index1)
   (index2 :initform nil :initarg :index2)
   (parameter-slots :initform '(index1 index2 table-param) :allocation :class)
   (table-param :initform "" :initarg :table-param)
   (indices :initform nil :initarg :indices)))
;; (defparameter *test* (make-instance 'table2D
;; 									:indices (list
;; 											  (list 0 0.1 0.2)
;; 											  (list 1.0101	2	10	25	50	100	200	1000))											  
;; 									:data (list 
;; 										   '(-2.326	0	1.282	1.751	2.054	2.326	2.576	3.09)
;; 										   '(-2.252	-0.017	1.292	1.785	2.107	2.4	2.67	3.235)
;; 										   '(-2.178	-0.033	1.301	1.818	20159	2.472	2.763	3.38))))

(defun interpolation-locate (seq i)
  "finds i in seq ordered in ascending order"
  (cond	((= i (first seq))
		 (values 0 0))
		(t 
		 (loop for x in (rest seq)
			with prev-x = (first seq) 
			for n from 1 do
			  (cond ((= i x)
					 (return (values n 0)))
					((or (< prev-x i x) (> prev-x i x))
					 (return (values (1- n) (/ (- i prev-x)
											   (- x prev-x))))))
			  (setf prev-x x)
			finally (return (values nil nil))))))

(defun interpolate-table2d (index1 index2 table)
  "Interpolate from table for index1 and index2"
  (with-slots (indices data interpolation) table
	(multiple-value-bind (i1 alpha1) (interpolation-locate (first indices) index1)
	  (unless i1
		  (error "Index value not in range of table"))
	  (if (and (not (first interpolation))
			   (not (= alpha1 0)))
		  (error "Exact value not found where interpolation was required"))

	  (multiple-value-bind (i2 alpha2) (interpolation-locate (second indices) index2)
		(unless i2
		  (error "Index value not in range of table"))
		(if (and (not (second interpolation))
				 (not (= alpha2 0)))
			(error "Exact value not found where interpolation was required"))

		(let* ((a (nth i2 (nth i1 data)))
			   (b (if (= alpha2 0)
					a
					(nth (1+ i2) (nth i1 data))))
			   (c (if (= alpha1 0)
					a
					(nth i2 (nth (1+ i1) data))))
			   (d (if (= alpha2 0)
					c
					(nth (1+ i2) (nth (1+ i1) data)))))
		  (let ((x1 (+ a (* alpha2 (- b a))))
				(x2 (+ c (* alpha2 (- d c)))))
			(+ x1 (* alpha1 (- x2 x1)))))))))

(defmethod eval-relation ((r table2d))
  (with-slots (index1 index2 table-param) r
	(- (value table-param)
	   (interpolate-table2d (value index1)
							(value index2)
							r))))

(defmethod solve-relation ((p parameter) (r table2d))
  ;; TODO emmit necessary errors
  (with-slots (index1 index2 table-param indices data) r
	(alexandria:switch (p :test #'eq)
	  (table-param (setf (value table-param)
						 (interpolate-table2d (value index1) (value index2) r)))
	  (index2
	   (multiple-value-bind (i1 alpha1) (interpolation-locate (first indices) (value index1))
		 (when i1
		   (let* ((interpolated-row (if (= alpha1 0)
									   (nth i1 data)
									   (mapcar (lambda (a b) (+ a (* alpha1 (- b a))))
											   (nth i1 data)
											   (nth (1+ i1) data)))))
			 (multiple-value-bind (ip alphap) (interpolation-locate interpolated-row (value table-param))
			   (let ((2a (nth ip (second indices)))
					 (2b (if (= alphap 0) 0 (nth (1+ ip) (second indices)))))
				 (setf (value index2)
					   (+ 2a (* alphap (- 2b 2a))))))))))
	  (index1
	   (multiple-value-bind (i2 alpha2) (interpolation-locate (second indices) (value index2))
		 (when i2
		   (let* ((interpolated-column (if (= alpha2 0)
										   (mapcar (lambda (row) (nth i2 row)) data)
										   (mapcar (lambda (row)
													 (let ((a (nth i2 row))
														   (b (nth (1+ i2) row)))
													   (+ a (* alpha2 (- b a)))))
												   data))))
			 (multiple-value-bind (ip alphap) (interpolation-locate interpolated-column (value table-param))
			   (let ((2a (nth ip (first indices)))
					 (2b (if (= alphap 0) 0 (nth (1+ ip) (first indices)))))
				 (setf (value index1)
					   (+ 2a (* alphap (- 2b 2a)))))))))))))
				 
			 
			 

(defclass tableN (relation)
  ((data :type :list :initarg :data :initform nil)
   (interpolation :initform t :initarg :interpolation)
   (table-param :initform "" :initarg :table-param)
   (index-params :initarg :index-params)
   (indices :initform nil :initarg :indices)))

(defun interpolate-tableN% (ialphas data)
  (macrolet ((with-ia (ia &rest body)
			   (alexandria:once-only (ia)
				 `(let ((i (first ,ia))
						(a (second ,ia)))
					,@body))))
	
	(cond
	  ((= (length ialphas) 1)
	   (with-ia (first ialphas)
		 (if (= a 0)
			 (nth i data)
			 (let ((d1 (nth i data))
				   (d2 (nth (1+ i) data)))
			   (+ d1 (* a (- d2 d1)))))))

	  (t
	   (with-ia (first ialphas)
		 (if (= a 0)
			 (interpolate-tableN% (rest ialphas)
								  (nth i data))
			 (let ((d1 (interpolate-tableN% (rest ialphas)
											(nth i data)))
				   (d2 (interpolate-tableN% (rest ialphas)
											(nth (1+ i) data))))
			   (+ d1 (* a (- d2 d1))))))))))

(defun interpolation-ias (index-values table)
  (with-slots (indices data) table
	(loop for index-value in index-values
	   for n from 0 
	   for index = (nth n indices)
	   with ialphas = nil do
		 (if index-value 
		   (multiple-value-bind (i alpha) (interpolation-locate index index-value)
			 (push (list i alpha) ialphas))
		   (push nil ialphas))
	   finally (return (reverse ialphas)))))

(defun interpolate-tableN (index-values table)
  (interpolate-tableN% (interpolation-ias index-values table)
					   (slot-value table 'data)))

;; (defun interpolate-index-tableN% (ialphas table-value table)
;;   (let* ((indices (slot-value table 'indices))
;; 		 (n (length indices))
;; 		 ;;(k (position nil ialphas))
;; 		 ;;(ith-cons-cell (nthcdr k ialphas))
;; 		 (kth-cons-cell (loop with c = ialphas do
;; 							 (if (eql (car c) nil)
;; 								 (return c)
;; 								 (prog (assert (eql (cdr c) nil))
;; 									(setf c (cdr c))))))
;; 		 (hashtable (make-hash-table :test 'eql))
;; 		 (dat (slot-value table 'data))
;; 		 (kth-index (nth k indices)))
;; 	;; Search 
;; 	(loop with i = 0
;; 	   with prev-value = nil 
;; 	   with  value = nil 
;; 	   for index-value = (nth i kth-index) do
		 
;; 		 (setf (car kth-cons-cell) index-value)
;; 		 (setf value (setf (gethash index-value)
;; 						   (interpolate-tablen% ialphas data)))
;; 		 (if (= value table-value)
;; 			 (return ))


(defun interpolate-index-tablen% (ialphas table-value table)
  (let* ((indices (slot-value table 'indices))
		 kth-index
		 (kth-cons-cell  (loop for ki in indices
							with c = ialphas do
							  (if (eql (car c) nil)
								  (progn
									(setf kth-index ki)
									(return c))
								  (setf c (cdr c)))
							finally (error "NIL not found")))
		 (data (slot-value table 'data))
		 (interpolated-data nil))
	(loop for index in kth-index
		 for i from 0 
	   for x = (progn
				 (rplaca kth-cons-cell (list i 0))
				 (interpolate-tablen% ialphas data)) do
		 (if (= x table-value)
			 (return-from interpolate-index-tablen% index))
		 (push x interpolated-data))
	(setf interpolated-data (nreverse interpolated-data))
	(print interpolated-data)
	(multiple-value-bind (i a) (interpolation-locate interpolated-data table-value)
	  (let ((i1 (nth i kth-index))
			(i2 (nth (1+ i) kth-index)))
		(+ i1 (* a (- i2 i1)))))))

(defun interpolate-index-tableN (index-values table-value table)
  (interpolate-index-tableN% (print (interpolation-ias index-values table))
							 table-value
							 table))

(define-relation inverse
    :parameters (a 1/a))

(defmethod eval-relation ((r inverse))
  (with-slots (a 1/a) r
    (- 1/a (/ a))))

(defmethod solve-relation ((p parameter) (r inverse))
  (with-slots (a 1/a) r
    (cond ((eql p a)
	   (setf (value a) (/ (value 1/a))))
	  ((eql p 1/a)
	   (setf (value 1/a) (/ (value a))))
	  (t (error "Invalid parameter ~a given to solve for relation ~a" p r)))))
