# system-solver

### Bibek Panthi (bpanthi977@gmail.com)

Solve for unknown parameters in a system of components. Each component is a set of parameters and relations. Relations relate parameters within same component or parameters among multiple components. 

```commmon-lisp
(defun three-reservoir-problem ()
  (let* ((p1 (make-instance 'pipe :name "p1" :r 15938 :p1 24))
	 (p2 (make-instance 'pipe :name "p2" :r 83565 :p1 8))
	 (p3 (make-instance 'pipe :name "p3" :r 170014 :p1 0))
	 (unknown (slot-value p1 'q)))
    (connect-pipes (list p1 p2 p3) (list t t t))
    (solve-for unknown)))
```

