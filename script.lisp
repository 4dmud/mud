(eval-when (:compile-toplevel)
  (unless (boundp '*started*)
    (load "util.lisp")))
(ffi:clines "#include \"lisp-internal.h\"")

(in-package :4d)

(defclass trigger ()
  ((rnum :type :integer :initarg :rnum :reader rnum)))

(with-c-struct (trigger (rnum trigger) "trig_index[#0]->~a" :int)
  (field vnum :int)
  (field pointer :pointer-void "proto")
  (field name :cstring "proto->name"))

(defmethod print-object ((trigger trigger) s)
  (format s "#<trigger ~d: ~a>" (vnum trigger) (name trigger)))

(define-condition trigger-not-found (error)
  ((vnum :initarg :vnum :reader vnum)))

(defmethod trigger ((vnum integer))
;;logarithmic search
  (loop
     with top_of_trigt = (oneliner () () :int "top_of_trigt")
     with bot = 0
     with top = (1- top_of_trigt)
     with rnum = (floor (/ top_of_trigt 2))
     for trig = (make-instance 'trigger :rnum rnum)
     do
       (cond ((= vnum (vnum trig))
	      (return trig))
	     ((= top bot)
	      (error 'trigger-not-found :vnum vnum))
	     ((< vnum (vnum trig))
	      (setf top (1- rnum)))
	     ((> vnum (vnum trig))
	      (setf bot (1+ rnum))))
       (setf rnum (floor (/ (+ top bot) 2)))))

(defclass variable ()
  ((pointer :initarg :pointer :reader pointer)))

(defmethod name ((variable variable))
  (oneliner ((pointer variable)) (:pointer-void) :cstring
	    "((struct trig_var_data*)#0)->name.c_str()"))


(defmethod value ((variable variable))
  (oneliner ((pointer variable)) (:pointer-void) :cstring
	    "((struct trig_var_data*)#0)->value.c_str()"))

(defmethod context ((variable variable))
  (oneliner ((pointer variable)) (:pointer-void) :long
	    "((struct trig_var_data*)#0)->context"))

(defmethod print-object ((variable variable) s)
  (format s "#<variable ~a(~d): ~a>" (name variable) (context variable) (value variable)))

(defmethod dg-variables ((entity entity))
  (if (script-pointer entity)
    (loop with var = (oneliner ((script-pointer entity)) (:pointer-void) :pointer-void
			       "((struct script_data*)#0)->global_vars")
       until (ffi:null-pointer-p var)
       collect (make-instance 'variable :pointer var)
       do (setf var (oneliner (var) (:pointer-void) :pointer-void
			      "((struct trig_var_data*)#0)->next")))))

(defmethod dg-variable ((entity entity) name &optional (context 0))
  (dolist (var (dg-variables entity))
    (if (and (string= name (name var))
	     (= context (context var)))
	(return var))))


(defmethod dg-value ((entity entity) name &optional (context 0))
  (let ((var (dg-variable entity name context)))
    (if var
	(value var))))

(defmethod dg-set-value ((entity entity) name &optional (context 0) (value ""))
  (declare (type string name value)
	   (type integer context))
  (if  (find #\. name)
       (error "illegal variable name.")
       (progn (4d-internal::dg-add-global-var (script-pointer entity) name value context)
	      value)))

(defsetf dg-value (entity name &optional (context 0)) (new-value)
  `(dg-set-value ,entity ,name ,context ,new-value))
