# system-solver

### Bibek Panthi (bpanthi977@gmail.com)

Solve for unknown parameters in a system/web of parameters and relations.

```commmon-lisp
;; Solves three nonlinear equations
(defun three-nonlinear-equations ()
  (with-parameters (a b c)
	(satisfying-relations 
		(lambda (a b c) (+ (expt a 3) (expt b 3/2) (expt c 0.2) -7.51532))
		(lambda (a b c) (+ (expt a 2) (* 3 a (/ b) c) -5.832))
		(lambda (a b c) (+ (expt a 4/3) (* c (log b) (/ (log 10))) (* a b) -5.9663)))
	(solve-for (list a b c))))

;; Determination of various parameters for Normal Depth
;; From hydraulics-book.lisp
(define-relation mannings-equation
    :parameters (q a 1/n p s0)
    :implicit (- q
		 (* 1/n a (expt (/ a p) 2/3) (sqrt s0))))

(defun trapezoidal-section ()
  (print "Enter parameters for trapezoidal-section (enter nil for unknown parameters)")
  (with-asked-parameters ((q "Discharge")
			  (y "Normal Depth")
			  (z "Side slope")
			  (b "Bottom width")
			  (n "Manning's n")
			  (s "Longitudinal slope"))
    (with-parameters (a p 1/n)
      (satisfying-relations (inverse :a n :1/a 1/n)
			    (lambda (a b y z)
			      (- a (* (+ b (* 2 y z)) y)))
			    (lambda (p b y z)
			      (- p (+ b (* 2 y (sqrt (1+ (* z z)))))))
			    (mannings-equation :s0 s :q q :1/n 1/n :a a :p p))
      (solve-for (list q y z b n s)))))

;; Pipe flow problems
;; From hydraulics-book.lisp
(defun pipe-problem ()
  (with-asked-parameters ((vel "Velocity")
			  (d "Diameter")
			  (e "Roughness")
			  (nu "Kinematic Viscosity (default 10^-6)" 1e-6)
			  (f "Friction factor")
			  (Re "Reynolds Number"))
    (make-instance 'pipe :vel vel :d d :e e :nu nu :f f :Re re)
    (solve-for (list vel d e f Re))))
```

