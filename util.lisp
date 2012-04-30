;;;utils.lisp: utility macros necessary at compile time
(defpackage :4d
  (:use :cl)
  (:shadow room))
(defpackage :4d-internal
  (:use :cl))

(in-package :4d)

(defmacro oneliner (&rest args)
  `(ffi:c-inline ,@args :side-effects nil :one-liner t))

(defmacro add-to-hook (fn hook)
  (declare (type symbol fn hook))
  `(push ,fn ,hook))

(defun run-hook (hook-name &optional arguments)
  (dolist (fn (symbol-value hook-name))
    (apply fn arguments)))

(defvar *current-character* nil
  "This variable is NIL, or set to the character that is currently being dealt with somehow.")

(defmacro with-paged-output (&body code)
  `(let ((*standard-output* (make-string-output-stream)))
     ,@code

     (page-string *current-character* (get-output-stream-string *standard-output*))))


(defvar *commands* (make-hash-table :test 'equal))

(defmacro defcommand (name (&key (min-level 0) (trust-groups nil)) &body code)
  `(eval-when (:load-toplevel)
     (handler-case
	 (setf (gethash ,(symbol-name name) *commands*)
	       (make-instance 'command
			      :fn #'(lambda (ch argument)
				      (declare (ignorable ch argument))
				      (handler-case (progn
						      ,@code)
					(error (e) (format t "error: ~a~%" e))))
			      :min-level ,min-level
			      :trust-groups ',trust-groups))
       (error (e) (error "error while compiling ~a: ~a" ',name e)))))