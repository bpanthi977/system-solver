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
   (factors :initform nil :initarg :factors)))

(defmethod solve-relation ((p parameter) (r continuity))
  (with-slots (parameters factors) r
    (loop for p2 in parameters
       for f in factors
       with p-f = nil
       with sum = 0 do
	 (if (eql p2 p)
	     (setf p-f f)
	     (incf sum (* f (value p2))))
       finally
	 (setf (value p) (* p-f (- sum))))))

(defmethod eval-relation ((r continuity))
  (with-slots (parameters factors) r
    (* 1 (loop for p in parameters
	    for f in factors
	    summing (* f (value p))))))

(defmethod add-discharge-connection ((p parameter) (c continuity) &optional (end t))
  "Add a pipe (its discharge) to a junction (to continuity equation)"
  (with-slots (parameters parameter-slots factors) c
    (push p parameters)
    (push (if end 1 -1) factors)
    (push c (relations p))))

(defun connect-pipes (pipes ends)
  "Connect pipes with at their respective ends (T for end of pipe, Nil for start of pipe)"
  (let ((discharge-relation (make-instance 'continuity)))
    (loop for p in pipes
       for endp in ends
       with prev-pipe-p = nil do 
	 (add-discharge-connection (slot-value p 'q) discharge-relation endp)
	 (when prev-pipe-p
	   (make-instance 'equal-pressure :p1 prev-pipe-p :p2 (slot-value p (if endp 'p2 'p1))))
	 (setf prev-pipe-p (slot-value p (if endp 'p2 'p1))))))
