;;;; package.lisp

(defpackage #:system-solver
  (:use #:cl)
  (:export
   ;; base
   #:parameter
   #:relation
   #:implicit-relation
   #:solve-relation
   #:eval-relation
   #:parameters
   #:value
   #:relations
   ;; interface
   #:define-relation
   #:define-component
   #:relation-implicit-managed
   #:with-parameters
   #:satisfying-relations
   #:component
   #:paramter-slots
   ;; system-solver
   #:solve-for))
   
