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

(defun game-loop-fn (data)
  (4d-internal::c-game-loop-fn data))

