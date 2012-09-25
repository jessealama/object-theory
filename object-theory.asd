
(in-package :cl-user)

(defpackage :object-theory-asd
  (:use :cl :asdf))

(in-package :object-theory-asd)

(defsystem :object-theory
  :description "Work with object theory."
  :author "Jesse Alama <jesse.alama@gmail.com>"
  :maintainer "Jesse Alama <jesse.alama@gmail.com>"
  :serial t
  :components ((:file "packages")
	       (:file "terms-and-formulas")
	       (:file "tautology")
	       (:file "principles")))
