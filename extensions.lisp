(in-package :system-solver)

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
