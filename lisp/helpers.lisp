(in-package :4d)

(defvar *current-character*)

(defun eval-string (player-name form-string)
  (let ((*standard-output* (player player-name))
	(*current-character* (player player-name)))
    (with-paged-output
	(princ (handler-case (eval (read-from-string form-string))
		 (error (e) (format nil "error: ~a" e)))))))