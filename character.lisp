(eval-when (:compile-toplevel)
  (unless (boundp '*started*)
    (load "util.lisp")))
(ffi:clines "#include \"lisp-internal.h\"")

(in-package :4d)

(defclass mobile (gray:fundamental-character-output-stream)
   ((pointer :initarg :pointer :reader pointer)))

(defmethod id ((mobile mobile))
  (oneliner ((pointer mobile)) (:pointer-void) :int "((Character*)#0)->id"))

(defmethod name ((mobile mobile))
  (oneliner ((pointer mobile)) (:pointer-void) :cstring "GET_NAME((Character*)#0)"))

(defmethod print-object ((mobile mobile) s)
  (format s "#<mobile ~a>" (name mobile)))

(defconstant +crlf+ (format nil "~c~c" (code-char 13) (code-char 10)))

(defmethod gray:stream-write-char ((mobile mobile) c)
  (4d-internal::send-to-char (pointer mobile)
			     (if (char= #\Newline c)
				 +crlf+
				 (princ-to-string c))))

;;very inefficient
(defun crlfize (string)
  (apply #'concatenate 'string
	 (loop for i from 0 to (1- (length string))
	    collect
	      (if (char= #\Newline (elt string i))
		  +crlf+
		  (princ-to-string (elt string i))))))

(defmethod gray:stream-write-string ((mobile mobile) s &optional (start 0) (end (1- (length s))))
  (4d-internal::send-to-char (pointer mobile)
			     (crlfize (subseq s start end))))

(defclass player (mobile) ())

(defmethod print-object ((player player) s)
  (format s "#<player ~a>" (name player)))


(defun for-each-character-fn (fn)
  (loop with character = (oneliner () () :pointer-void "character_list")
     do (funcall fn (make-instance (if (oneliner (character) (:pointer-void) :bool "IS_NPC((Character*)#0)")
				       'mobile
				       'player)
				   :pointer character))
     until (ffi:null-pointer-p (setf character (oneliner (character) (:pointer-void) :pointer-void "((Character*)#0)->next")))))

(defmacro for-each-character ((character) &body body)
  (declare (type symbol character))
  `(block nil
     (for-each-character-fn #'(lambda (,character)
			     ,@body))))

(defun all-characters ()
  (let ((characters nil))
    (for-each-character (c)
			(push c characters))
    (reverse characters)))

(defun for-each-player-fn (fn)
  (loop with descriptor = (oneliner () () :pointer-void "descriptor_list")
     when (oneliner (descriptor) (:pointer-void) :bool "IS_PLAYING((Descriptor*)#0) && ((Descriptor*)#0)->character")
     do (funcall fn (make-instance 'player
				   :pointer (oneliner (descriptor) (:pointer-void) :pointer-void "((Descriptor*)#0)->character")))
      until (ffi:null-pointer-p (setf descriptor (oneliner (descriptor) (:pointer-void) :pointer-void "((Descriptor*)#0)->next")))))

(defmacro for-each-player ((player) &body body)
  (declare (type symbol player))
  `(block nil
     (for-each-player-fn #'(lambda (,player)
			     ,@body))))

(defun players ()
  (let ((players nil))
    (for-each-player (p)
		     (push p players))
    (reverse players)))

(defmethod player ((id integer))
  (or (for-each-player (p)
		       (if (= id (id p))
			   (return p)))
      (error "No such player.")))

(defmethod player ((name string))
  (or (for-each-player (p)
		       (if (string= (string-upcase name) (string-upcase (name p)))
			   (return p)))
      (error "No such player.")))