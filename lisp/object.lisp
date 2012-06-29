(ffi:clines #.(format nil "#include \"~a\"" (asdf:system-relative-pathname :4d-lisp "../lisp-internal.h")))
(in-package :4d)

(defclass obj () ())

(defclass object-prototype (obj)
  ((vnum :initarg :vnum :reader vnum)))

(defmacro object-field (object name type)
  (declare (type symbol type)
	   (type string name))
  `(oneliner ((pointer ,object)) (:pointer-void) ,type
	     ,(format nil "((struct obj_data *)#0)->~a" name)))

(defclass object-instance (obj entity)
  ((id :initarg :id :reader id)))



(defmethod print-object ((obj obj) s)
  (format s "#<~a ~d: ~a>"
	  (string-downcase (symbol-name (type-of obj)))
	  (vnum obj)
	  (short-description obj)))

(defmethod name ((object obj))
  (object-field object "name" :cstring))

(defmethod short-description ((object obj))
  (object-field object "short_description" :cstring))

(defun obj-rnum-to-vnum (rnum)
  (oneliner (rnum) (:int) :int
	    "obj_index[#0].vnum"))

(defmethod vnum ((object object-instance))
  (obj-rnum-to-vnum
   (object-field object "item_number" :int)))

(defun obj-vnum-to-rnum (vnum)
  (oneliner (vnum) (:int) :int
	    "obj_vTor[#0]"))

(defmethod rnum ((object object-prototype))
  (obj-vnum-to-rnum (vnum object)))

(defmethod pointer ((object object-prototype))
  (oneliner ((rnum object)) (:int) :pointer-void
	    "&obj_proto[#0]"))

(defmethod pointer ((object object-instance))
  (oneliner ((id object)) (:int) :pointer-void
	    "object_list[#0]"))

(define-condition object-prototype-not-found (error) ((vnum :initarg :vnum :reader vnum)))

(defmethod object-prototype ((object object-instance))
  (object-prototype (vnum object)))

(defmethod prototype ((object object-instance))
  (object-prototype object))

(defun object-prototype-by-rnum (rnum)
  (object-prototype (obj-rnum-to-vnum rnum)))

(defun all-objects ()
  (let ((objects nil))
    (ffi:c-inline (#'(lambda (id)
		       (push (object-instance id) objects))) (:function) :void
"for (olt_it ob = object_list.begin();ob != object_list.end(); ob++)
cl_funcall(2, #0, MAKE_FIXNUM(GET_ID(ob->second)));")
    (reverse objects)))

(defmethod triggers ((object object-prototype))
  (unless (ffi:null-pointer-p (object-field object "proto_script" :pointer-void))
    (loop for i from 0 to (1- (object-field object "proto_script->size()" :int))
       nconc (handler-case
		 (list (trigger (oneliner ((pointer object) i) (:pointer-void :int) :int
				  "((struct obj_data *)#0)->proto_script->at(#1)")))
	       (trigger-not-found (e) (warn "trigger not found: ~d" (vnum e)) nil)))))

(defun all-object-prototypes ()
  (loop for i from 1 to (1- (oneliner () () :int "top_of_objt"))
       for vnum = (oneliner (i) (:int) :int
					   "obj_index[#0].vnum")
       collect (object-prototype vnum)))

(defmethod script-pointer ((object object-instance))
  (let ((ptr (object-field object "script" :pointer-void)))
    (unless (ffi:null-pointer-p ptr)
      ptr)))

(defmethod ptr-to-object-instance (ptr)
  (object-instance
   (oneliner (ptr) (:pointer-void) :int
	     "((struct obj_data *)#0)->id")))

(defmethod contents ((object object-instance))
  (let ((contains
	 (object-field object "contains" :pointer-void)))
    (when contains
      (loop with c = contains
	   until (ffi:null-pointer-p c)
	   collect (ptr-to-object-instance c)
	   do
	   (setf c (oneliner (c) (:pointer-void) :pointer-void
			     "((struct obj_data *)#0)->next_content"))))))

(defmethod object-val ((object obj) num)
  (when (< num 0)
    (error "num can not be negative."))

  (when (>= num (oneliner () () :int "NUM_OBJ_VAL_POSITIONS"))
    (error "num too high"))

  (oneliner ((pointer object) num) (:pointer-void :int) :int
	    "((struct obj_data*)#0)->obj_flags.value[#1]"))

(defun obj-to-object-type (obj)
  (num-to-object-type
   (object-field obj "obj_flags.type_flag" :int)))

(defun obj-proto-class (obj)
  (let ((class
	 (values
	  (intern (concatenate 'string
			       (symbol-name (obj-to-object-type obj))
			       "-OBJECT-PROTO")))))
    (handler-case (and (find-class class)
		       class)
      (error () 'undefined-object-proto))))

(defun obj-instance-class (obj)
  (let ((class
	 (values
	  (intern (concatenate 'string
			       (symbol-name (obj-to-object-type obj))
			       "-OBJECT-INSTANCE")))))
    (handler-case (and (find-class class)
		       class)
      (error () 'undefined-object-instance))))


(defmethod object-prototype ((vnum integer))
  (let ((rnum (obj-vnum-to-rnum vnum)))
    (if (= 0 rnum)
	(error 'object-prototype-not-found :vnum vnum)
	(let ((proto
	       (make-instance 'object-prototype :vnum vnum)))
	  (change-class proto (obj-proto-class proto))))))

(defmethod object-instance ((id integer))
  (let ((instance
	 (make-instance 'object-instance
			:id id)))
    (change-class instance (obj-instance-class instance))))
