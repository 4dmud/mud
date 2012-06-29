(in-package :4d)

(defconstant +startup-file+ "lisp/4d-startup.lisp")

(defun init ()
  (4d-internal::mud-log "initializing lisp.~%")
  (in-package :4d)
  (defvar *started* t)
  (if (probe-file +startup-file+)
      (load +startup-file+)
      (4d-internal::mud-log "No startup lisp file found. continuing..~%")))
