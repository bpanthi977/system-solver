;;;; system-solver.asd

(asdf:defsystem #:system-solver
  :description "Describe system-solver here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (:alexandria :screamer)
  :components ((:file "package")
               (:file "system-solver")))
