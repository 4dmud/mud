;; Events are interesting things that happen in 4d, that we might or might not want to dynamically respond to.
;; Rather than hardcoding a particular function to be run at the point where the event happens,
;; this event point should instead signal one of the conditions defined here.
(ffi:clines "#include \"../lisp-internal.h\"")
(in-package :4d)

(define-condition player-condition (condition)
  ((player :initarg :player :accessor player)))

(defmethod shared-initialize :around ((c player-condition) slot-names &rest initargs &key player &allow-other-keys)
  "ensure that if a pointer is passed, it is converted to a player character. This allows easier use from c++."
  (when (si:foreign-data-p player)
    (setf (getf initargs :player)
	  (4d::ptr-to-character player)))
  (apply #'call-next-method c slot-names initargs))

(define-condition player-login (player-condition) ())
(define-condition player-logout (player-condition) ())

(defvar *toplevel-handlers* nil)

(defun build-toplevel-handler-fn ()
  (eval `(lambda (fn &rest arguments)
	   (handler-bind ,(mapcar #'(lambda (entry)
				      (destructuring-bind (condition function) entry
					`(,condition (function ,function))))
				  *toplevel-handlers*)
			 (apply fn arguments)))))

(defvar *toplevel-handler-fn* (build-toplevel-handler-fn))

(defun rebuild-toplevel-handler-fn ()
  (setf *toplevel-handler-fn* (build-toplevel-handler-fn)))

(defun handler-binding-equal? (condition handler)
  (lambda (entry)
    (destructuring-bind (c h) entry
      (and (eq c condition)
	   (eq h handler)))))

(defun add-toplevel-handler (condition handler)
  (declare (type (symbol condition handler)))
  (symbol-function handler) ;;errors if there's no function
  (when (find-if (handler-binding-equal? condition handler)
		 *toplevel-handlers*)
    (error "handler already set."))
  
  (push (list condition handler)
	*toplevel-handlers*)

  (rebuild-toplevel-handler-fn))

(defun clear-toplevel-handlers ()
  (setf *toplevel-handlers* nil)
  
  (rebuild-toplevel-handler-fn))

(defun remove-toplevel-handler (condition handler)
  (setf *toplevel-handlers*
	(delete-if (handler-binding-equal? condition handler)
		   *toplevel-handlers*))

  (rebuild-toplevel-handler-fn))

(defun game-loop-fn (data)
  (funcall *toplevel-handler-fn* #'4d-internal::c-game-loop-fn data))

