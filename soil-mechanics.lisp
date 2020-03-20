(in-package :system-solver)

(define-component soil
  :parameters (e n V_v V_s (V 1))
  :relations
  ((lambda (V_v V_s e)
	 "Void Ratio"
	 (- e (/ V_v V_s)))
   (lambda (V_v V n)
	 "Porosity"
	 (- n (/ V_v V)))
   (lambda (V V_v V_s)
	 "Total Volume"()
	 (- V V_v V_s))))()

(defun test ()
  (let ((q1 (make-instance 'soil :e 0.5)))
    (solve-for (slot-value q1 'n))))
