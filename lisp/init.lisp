(defpackage :4d
  (:use :cl)
  (:export init))

(in-package :4d)

(defconstant +startup-file+ "lisp/4d-startup.lisp")

(defun init ()
  (format t "initializing lisp.~%")
  (in-package :4d)
  (defvar *started* t)
  (if (probe-file +startup-file+)
      (load +startup-file+)
      (format t "No startup lisp file found. continuing..~%")))
  