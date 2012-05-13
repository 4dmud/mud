(eval-when (:compile-toplevel)
  (unless (boundp '*started*)
    (load "util.lisp")))

(in-package :4d)

(defun animalsay (animal ch victim text)
  (if (and ch (string= (name victim) (name ch)))
      (format t "don't send yourself such things!~%")
      (let ((message (format nil "~{~a~%~}"
			     (loop with p = (ext:run-program "cowsay" `("-f" ,animal ,text))
				for line = (read-line p nil)
				while line collect line))))
	(format victim "~a has sent you this message:~%" (if ch (name ch) "A mysterious force"))
	(format victim "~a" message)
	(when ch
	  (format ch "You send ~a this message:~%" (name victim))
	  (format ch "~a" message)))))

(macrolet ((make-animal-commands (&rest commands)
	     `(progn ,@(loop for command in commands collect
			    `(defcommand ,(car command) (:min-level 55)
			       (let ((pos (position #\Space argument :start 1)))
				 (animalsay ,(cadr command) ch (find-player-by-incomplete-name (subseq argument 1 pos)) (subseq argument (1+ pos)))))))))
  (make-animal-commands (cowsay "default") (dragonsay "dragon-and-cow") (bunnysay "bunny")))
