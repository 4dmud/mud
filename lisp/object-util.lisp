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

