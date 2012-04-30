;;;triggers.lisp: the 4d c++ code calls these when events happen, much like it does for dg.
;;;each trigger function does a bunch of things pre-defined at compile time, and also runs hooks that can be set at runtime for maximum flexibility.

(eval-when (:compile-toplevel)
  (unless (boundp '*started*)
    (load "util.lisp")))

(defpackage :4d
  (:use :cl))

(in-package :4d)
(defvar *command-hook* nil
  "hook run when commands are typed by mobiles or characters. This contains a list of functions, each of which is expected to take three arguments: the character, the command and the command argument")

(defun command-trigger (ch-ptr command argument &aux (character (ptr-to-character ch-ptr)))
  "run for each command typed by both mobiles and characters."
  (declare (type string command argument))
  (if (typep character 'player)
      (format t "~a typed ~a ~a.~%" (name character) command argument))
  (dolist (fn *command-hook*)
    (funcall fn character command argument))

  (if (run-command character command argument)
      1
      0))
