(in-package :system-solver)

(define-component soil
    :parameters (e n V_v V_s V)
    :relations ((:implicit (- e (/ V_v V_s)) :on (V_v V_s e) :name "Void Ratio")
		(:implicit (- n (/ V_v V)) :on (V_v V n) :name "Porosity")
		(:implicit (- V V_v V_s) :on (V V_v V_s) :name "Total volume")))

(defun test ()
  (let ((q1 (make-instance 'soil :e 0.5 :V 1)))
    (solve-for (slot-value q1 'n))))
