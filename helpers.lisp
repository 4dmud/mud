(defpackage :4d
  (:use :cl)
  (:export mud-log eval-string))
(in-package :4d)

(eval-when (:compile-toplevel)
  (unless (boundp '*started*)
    (load "util.lisp")))

(defvar *current-character*)

(defun eval-string (player-name form-string)
  (let ((*standard-output* (player player-name))
	(*current-character* (player player-name)))
    (with-paged-output
	(princ (handler-case (eval (read-from-string form-string))
		 (error (e) (format nil "error: ~a" e)))))))