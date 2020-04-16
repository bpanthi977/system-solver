;;;; system-solver.asd

(asdf:defsystem #:system-solver
  :description "Describe system-solver here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (:alexandria :serapeum :screamer :split-sequence :gsll :anaphora)
  :components ((:file "package")
			   (:file "solver")
			   (:file "base")
               (:file "system-solver")
			   (:file "interface")
			   (:file "extensions")
			   (:file "designs")))

