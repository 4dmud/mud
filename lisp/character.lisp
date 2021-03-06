;;(ffi:clines #.(format nil "#include \"~a\"" (asdf:system-relative-pathname :4d-lisp "../lisp-internal.h")))
(ffi:clines "#include \"../lisp-internal.h\"")
(defpackage :4d
  (:use :cl)
  (:shadow room))

(defpackage :4d-internal (:use :cl))

(in-package :4d)

(defclass mobile (gray:fundamental-character-output-stream entity)
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

(defmethod prototype ((mobile mobile))
  (mobile-prototype mobile))

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
       with d
     until (ffi:null-pointer-p descriptor)
     do (setf d descriptor) (setf descriptor (oneliner (d) (:pointer-void) :pointer-void "((Descriptor*)#0)->next"))
     when (oneliner (d) (:pointer-void) :bool "IS_PLAYING((Descriptor*)#0) && ((Descriptor*)#0)->character")
     do (funcall fn (make-instance 'player
				   :pointer (oneliner (d) (:pointer-void) :pointer-void "((Descriptor*)#0)->character")))))


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

(defmethod triggers ((mobile mobile-prototype))
  (unless (ffi:null-pointer-p (mobile-field mobile "proto_script" :pointer-void))
    (loop for i from 0 to (1- (mobile-field mobile "proto_script->size()" :int))
       nconc (handler-case
		   (list (trigger (oneliner ((pointer mobile) i) (:pointer-void :int) :int
					    "((Character *)#0)->proto_script->at(#1)")))
		 (trigger-not-found (e) (warn "trigger not found: ~d" (vnum e)) nil)))))

(defun all-mobile-prototypes ()
  (let* ((protos nil)
	 (fn #'(lambda (vnum pointer)
		 (unless (ffi:null-pointer-p pointer)
		   (push (mobile-prototype vnum) protos)))))
    (ffi:c-inline (fn) (:function) :void
		  "for (map<mob_vnum, Character *>::iterator it = mob_proto.begin(); it != mob_proto.end(); it++)
  cl_funcall(3, #0, MAKE_FIXNUM(it->first), ecl_make_pointer(it->second));")
    (reverse protos)))

(defmethod script-pointer ((mobile mobile))
  (let ((ptr (oneliner ((pointer mobile)) (:pointer-void) :pointer-void
		       "SCRIPT((Character *)#0)")))
    (unless (ffi:null-pointer-p ptr)
      ptr)))

(defmethod dg-set-value ((entity player) name &optional context (value ""))
  (declare (type string name value)
	   (type integer context))
  "players only have context 0, so any context given here should be forcibly set to 0"
  (call-next-method entity name 0 value)
  (4d-internal::player-save-vars (pointer entity)))

(defun find-player-by-incomplete-name (name)
  (declare (type string name))
  (if (= 0 (length name))
      (error "Please give a name."))
  (dolist (p (sort (players) #'(lambda (x y)
				 (string< (name x) (name y)))))
    (if (and (>= (length (name p))
		(length name))
	     (string= (string-upcase name)
		      (string-upcase (subseq (name p) 0 (length name)))))
	(return p))))
    
(defmethod inventory ((mobile mobile))
  (loop with ptr = (mobile-field mobile "carrying" :pointer-void)
       until (ffi:null-pointer-p ptr)
       collect (ptr-to-object-instance ptr)
       do (setf ptr (oneliner (ptr) (:pointer-void) :pointer-void
			      "((struct obj_data *)#0)->next_content"))))
       
(defmethod contents ((mobile mobile))
  (inventory mobile))

(defun force (character command)
  (4d-internal::command-interpeter (pointer character) command))

(defmethod afk-message ((player player))
  (oneliner ((pointer player)) (:pointer-void) :cstring "AFK_MSG ( (Character*)#0 )"))

(defmethod set-afk-message ((player player) message)
  (ffi:c-inline ((pointer player) message) (:pointer-void :cstring) :cstring
		"AFK_MSG ((Character *)#0)=strdup(#1);"))

(defsetf afk-message set-afk-message)
