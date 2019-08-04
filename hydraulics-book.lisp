(defun siphon ()
  (let ((pipe1 (make-instance 'pipe :z1 9 :z2 11.5 :l 5 :d 0.1 :f 0.08 :k .5 :p1 100 :name "pipe1"))
	(pipe2 (make-instance 'pipe :l 10 :d 0.1 :f 0.08 :k 1 :z2 5 :p2 100 :name "pipe2"))
	system)
    (connect-pipes (list pipe1 pipe2) (list t nil))
    (setf system (solve-for (list (slot-value pipe1 'vel)
				  (slot-value pipe1 'p2))))
    (let ((vapour-pressure 70))
      (when (< (value (slot-value pipe1 'p2)) vapour-pressure)
	(print "Minimum pressure (at summit) is less than vapour-pressure")))
    system))

(define-relation mannings-equation
    :parameters (q a 1/n p s0)
    :implicit (- q
		 (* 1/n a (expt (/ a p) 2/3) (sqrt s0))))

(defun normal-depth ()
  (with-parameters (a p y (b 2) (q 1) (1/n 48.479))
    (satisfying-relations (lambda (a b y)
			    (- a (* b y)))
			  (lambda (p b y)
			    (- p (* 2 y) b))
			  (mannings-equation :s0 0.0025 :q q :1/n 1/n :a a :p p))
    (solve-for (list y))
    (value y)))


