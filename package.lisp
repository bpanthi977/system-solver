;;;; package.lisp

(defpackage #:system-solver
  (:use #:cl)
  (:export
   ;; base
   #:parameter
   #:name
   #:value
   #:relations
   
   #:relation
   #:parameters

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
   #:parameter-slots
   ;; system-solver
   #:solve-for
   ;;extensions
   #:inverse
   #:data
   #:index1
   #:index2
   #:indices
   #:table-param
   #:table2d
   #:interpolate-table2d
   #:interpolate-tableN))
   
