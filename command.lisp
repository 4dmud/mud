(eval-when (:compile-toplevel)
  (unless (boundp '*started*)
    (load "util.lisp")))
(defpackage :4d
  (:use :cl))

(in-package :4d)
(ffi:clines "#include \"lisp-internal.h\"")

(defclass command ()
  ((fn :initarg :fn :reader fn)
   (min-level :initarg :min-level :initform 0 :reader min-level)
   (trust-groups :initarg :trust-groups :initform nil :reader trust-groups)))



(defmacro make-trust-value-fn (&rest trusts)
  `(defun trust-value (command)
     (apply #'+
	    (loop for trust in (trust-groups command)
	       collect (case trust
			 ,@(loop for trust in trusts collect
				`(,(intern (symbol-name trust) :keyword)
				   (oneliner () () :int
					     ,(format nil "WIZ_~a_GRP" trust)))))))))

(make-trust-value-fn :ban :dspln :edit :heal :house :imm1 :imm2 :impl :kill :load :olc :quest :sen :tele :trig :marry :goto :global :hedit :imm3)

(defun can-run-command (character command)
  (and command
       (>= (level character) (min-level command))
       (or (= (level character) (oneliner () () :int "LVL_IMPL"))
	   (/= 0 (logand (trust-value command)
			 (cmd-flags character))))))

(defun run-command (character command argument)
  (let ((cmd (gethash (string-upcase command) *commands*)))
    (if (can-run-command character cmd)
	(let ((*standard-output* character))
	  (funcall (fn cmd) character argument)
	  t)
	nil)))

(defun commands-for (character)
  (let ((commands nil))
    (maphash #'(lambda (name command)
		 (if (can-run-command character command)
		     (push name commands)))
	     *commands*)
    (sort commands #'string<)))


(defcommand lisp-commands (:min-level 52)
  (format t "The following lisp commands are available to you:~%")
  (dolist (command (commands-for ch))
    (format t "~a~%" (string-downcase command))))