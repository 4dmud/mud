(eval-when (:compile-toplevel)
  (unless (boundp '*started*)
    (load "util.lisp")))

(defpackage :4d-internal
  (:use :cl))

(in-package :4d-internal)


(ffi:clines "#include \"lisp-internal.h\""
	    "void send_to_char(const char*, Character*);"
	    "void page_string (Descriptor*, char*, int);")

(defun send-to-char (ch string)
  (ffi:c-inline ((ffi:convert-to-cstring string) ch)
		(:cstring :pointer-void) :void
		"send_to_char(#0, (Character *)#1);"
		:side-effects t))

(defun page-string (ch string)
  (ffi:c-inline (ch (ffi:convert-to-cstring string))(:pointer-void :cstring) :void
		"page_string (((Character *) #0)->desc, #1, TRUE);"))

(ffi:clines "void basic_mud_log ( const char *format, ... );")
(defun mud-log (&rest args)
  (ffi:c-inline ((apply #'format nil args)) (:cstring) :void
		"basic_mud_log(#0);"))