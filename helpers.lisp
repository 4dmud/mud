(defpackage :4d
  (:use :cl)
  (:export mud-log eval-string))
(in-package :4d)

(ffi:clines "void basic_mud_log ( const char *format, ... );")
(defun mud-log (&rest args)
  (ffi:c-inline ((apply #'format nil args)) (:cstring) :void
		"basic_mud_log(#0);"))

(defun eval-string (form-string)
  (princ-to-string (handler-case (eval (read-from-string form-string))
		     (error (e) (format nil "error: ~a" e)))))