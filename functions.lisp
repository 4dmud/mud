(eval-when (:compile-toplevel)
  (unless (boundp '*started*)
    (load "util.lisp")))

(in-package :4d-internal)


(ffi:clines "#include \"lisp-internal.h\""
	    "void send_to_char(const char*, Character*);")

(defun send-to-char (ch string)
  (ffi:c-inline ((ffi:convert-to-cstring string) ch)
		(:cstring :pointer-void) :void
		"send_to_char(#0, (Character *)#1);"
		:side-effects t))
