(eval-when (:compile-toplevel)
  (unless (boundp '*started*)
    (load "util.lisp")))
(ffi:clines "#include \"lisp-internal.h\"")

(defpackage :4d
  (:use :cl)
  (:shadow room))

(defpackage :4d-internal (:use :cl))

(in-package :4d)

(defclass mobile (gray:fundamental-character-output-stream)
   ((pointer :initarg :pointer :reader pointer)
    (buffer :initform nil :accessor mobile-buffer)))

(defmacro mobile-field (mobile name type)
  (declare (type symbol type)
	   (type string name))
  `(oneliner ((pointer ,mobile)) (:pointer-void) ,type
	     ,(format nil "((Character *)#0)->~a" name)))


(defmethod id ((mobile mobile))
  (mobile-field mobile "id" :int))

(defmethod name ((mobile mobile))
  (oneliner ((pointer mobile)) (:pointer-void) :cstring "GET_NAME((Character*)#0)"))

(defmethod vnum ((mobile mobile))
  (mobile-field mobile "vnum" :int))

(defmethod level ((mobile mobile))
  (oneliner ((pointer mobile)) (:pointer-void) :int "GET_LEVEL((Character*)#0)"))

(defmethod print-object ((mobile mobile) s)
  (format s "#<mobile ~a>" (name mobile)))

(defconstant +crlf+ (format nil "~c~c" (code-char 13) (code-char 10)))

(defmethod gray:stream-write-char ((mobile mobile) c)
  (case (length (mobile-buffer mobile))
    (0
     (if (char= c #\{)
	 (push c (mobile-buffer mobile))
	 (4d-internal::send-to-char (pointer mobile)
				    (case c
				      (#\Newline +crlf+)
				      (t (princ-to-string c))))))

    (1
     (if (char= c #\{)
	 (progn
	   (setf (mobile-buffer mobile) nil)
	   (4d-internal::send-to-char (pointer mobile) "{{"))
	 (push c (mobile-buffer mobile))))
    (2
     (push c (mobile-buffer mobile))
     (4d-internal::send-to-char (pointer mobile) (concatenate 'string
							      (reverse (mobile-buffer mobile))))
     (setf (mobile-buffer mobile) nil))))

(defmethod gray:stream-finish-output ((mobile mobile))
  (setf (mobile-buffer mobile) nil))


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


(defclass mobile-prototype (mobile) ())

(defmethod print-object ((mobile mobile-prototype) s)
  (format s "#<mobile-prototype ~d: ~a>" (vnum mobile) (name mobile)))

(define-condition mobile-prototype-not-found (error)
  ((vnum :initarg :vnum :reader vnum)))

(defmethod mobile-prototype ((vnum integer))
  (let ((pointer (oneliner (vnum) (:int) :Pointer-void
			   "mob_proto[#0]")))
    (if (ffi:null-pointer-p pointer)
	(error 'mobile-prototype-not-found :vnum vnum)
	(make-instance 'mobile-prototype
		       :pointer pointer))))


(defmethod mobile-prototype ((mobile mobile))
  (mobile-prototype (vnum mobile)))

(defclass player (mobile) ())

(defmethod print-object ((player player) s)
  (format s "#<player ~a>" (name player)))


(defun ptr-to-character (ptr)
  (make-instance (if (oneliner (ptr) (:pointer-void) :bool
			       "IS_NPC((Character*) #0)")
		     'mobile
		     'player)
		 :pointer ptr))

(defun for-each-character-fn (fn)
  (loop with character = (oneliner () () :pointer-void "character_list")
     until (ffi:null-pointer-p character)
     do (funcall fn (ptr-to-character character))
       (setf character (oneliner (character) (:pointer-void) :pointer-void "((Character*)#0)->next"))))


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
     until (ffi:null-pointer-p descriptor)
     when (oneliner (descriptor) (:pointer-void) :bool "IS_PLAYING((Descriptor*)#0) && ((Descriptor*)#0)->character")
     do (funcall fn (make-instance 'player
				   :pointer (oneliner (descriptor) (:pointer-void) :pointer-void "((Descriptor*)#0)->character")))
       (setf descriptor (oneliner (descriptor) (:pointer-void) :pointer-void "((Descriptor*)#0)->next"))))


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

(defmethod page-string ((mobile mobile) string)
  (4d-internal::page-string (pointer mobile) string))

(defmethod cmd-flags ((player player))
  (oneliner ((pointer player)) (:pointer-void) :int
	    "CMD_FLAGS((Character*)#0)"))

(defmethod cmd-flags ((mobile mobile))
  (declare (ignore mobile))
  0)

(defmethod lisp-commands-for ((character player))
  (apply #'nconc
	 (maphash #'(lambda (k v)
		      (if (can-run-command character v)
			  (list k)
			  nil))
		  *commands*)))