(in-package :system-solver)

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
    :implicit (- hf (* (signum Q) r (expt Q 2))))

(defmethod solve-relation ((var symbol) (r head-loss))
  ;; This method is actually not necessary, but is defined for reference
  (with-slots (r q hf) r
    (cond ((eql var 'hf)
	   (setf (value hf) (* (signum (value q)) (value r) (expt (value q) 2))))
	  ((eql var 'q)
	   (setf (value q) (* (signum (value hf)) (sqrt (abs (/ (value hf) (value r)))))))
	  ((eql var 'r)
	   (setf (value r) (abs (/ (value hf) (expt (value q) 2))))))))

(define-relation head-loss-2
    :parameters (hf p1 p2)
    :implicit (+ hf (- p1) p2)
    :name "Head loss depending on pressure difference")

(define-relation friction-factor*
    :name "1/sqrt(Friction factor) f*(k,Re)"
    :parameters (f* k Re D)
    :implicit (if (< Re 2000)
		  (- f* (sqrt (/ Re 64))) ;; Laminar case
		  (+ f*  ;; Colebrook's equation
		     (* 2 (/ (log 10)) (log (+ (/ k D 3.71) (* 2.523 f* (/ Re) )))))))

(define-relation friction-factor->f*
    :name "Friction factor and 1/sqrt(f)"
    :parameters (f f*)
    :implicit (- f (/ (expt f* 2))))

(define-relation resistance-coeff
    :name "Resistance Coefficient r(f, L, D)"
    :parameters (r f L D)
    :implicit (- r (* 8 f l (/ 1 (expt pi 2) 9.81 (expt D 5)))))

(define-component pipe
    :parameters (Re vel D nu Q A r hf p1 p2 f k L f*)
    :relations ((:relation pipe-discharge
			   :parameters (:Q q :v vel :d d))
		(:relation reynolds-number
			   :parameters (:Re Re :v vel :nu nu :D D))
		(:relation head-loss
			   :parameters (:hf hf :r r :q q))
		(:relation head-loss-2
			   :parameters (:hf hf :p1 p1 :p2 p2))
		(:relation friction-factor*
			   :parameters (:f* f* :Re Re :k k :D D))
		(:relation friction-factor->f*
			   :parameters (:f f :f* f*))
		(:relation resistance-coeff
			   :parameters (:f f :r r :l L :D D))))

(define-relation equal-pressure
    :name "Equal pressure at two point or a junction"
    :parameters (p1 p2)
    :implicit (- p1 p2))

;;
;; SAMPLE: Relation without implicit formula (Continuity equation)
;;
(defclass continuity (relation)
  ((name :initform "Continuity at a junction" :allocation :class)
   (parameter-slots :initform nil)
   (factors :initform nil :initarg :factors)))

(defmethod add-discharge-connection ((p parameter) (c continuity) &optional (end t))
  (with-slots (parameters parameter-slots factors) c
    (push p parameters)
    (push (if end 1 -1) factors)
    (push c (relations p))
    (let ((f (first parameter-slots)))
      (push (if f (1+ f) 1) parameter-slots))))

(defun connect-pipes (pipes ends)
  (let ((discharge-relation (make-instance 'continuity)))
    (loop for p in pipes
       for endp in ends
       with prev-pipe-p = nil do 
	 (add-discharge-connection (slot-value p 'q) discharge-relation endp)
	 (when prev-pipe-p
	   (make-instance 'equal-pressure :p1 prev-pipe-p :p2 (slot-value p (if endp 'p2 'p1))))
	 (setf prev-pipe-p (slot-value p (if endp 'p2 'p1))))))

(defmethod solve-relation ((var number) (r continuity))
  ;; TODO remove this method and don't use paramter-slots
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

(defmethod solve-relation ((p parameter) (r continuity))
  (solve-relation (- (length (parameters r))
		     (position p (parameters r)))
		  r))

(defmethod eval-relation ((r continuity))
  (with-slots (parameters factors) r
    (* 1 (loop for p in parameters
	    for f in factors
	    summing (* f (value p))))))

