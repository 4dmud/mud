(eval-when (:compile-toplevel)
  (unless (boundp '*started*)
    (load "util.lisp")))
(ffi:clines "#include \"lisp-internal.h\"")

(in-package :4d)

(defclass trigger ()
  ((rnum :type :integer :initarg :rnum :reader rnum)))

(defmethod vnum ((trigger trigger))
  (oneliner ((rnum trigger)) (:int) :int "trig_index[#0]->vnum"))

(defmethod pointer ((trigger trigger))
  (oneliner ((rnum trigger)) (:int) :pointer-void "trig_index[#0]->proto"))

(defmethod name ((trigger trigger))
  (oneliner ((rnum trigger)) (:int) :cstring "trig_index[#0]->proto->name"))

(defmethod print-object ((trigger trigger) s)
  (format s "#<trigger ~d: ~a>" (vnum trigger) (name trigger)))

(define-condition trigger-not-found (error) ())

(defun for-each-trig-fn (fn)
  (loop for rnum from 0 to (1- (oneliner () () :int "top_of_trigt")) do
       (funcall fn (make-instance 'trigger :rnum rnum))))

(defmacro for-each-trigger ((trigger) &body body)
  `(for-each-trig-fn #'(lambda (,trigger)
			   ,@body)))

(defun top-of-trigt ()
  (oneliner () () :int "top_of_trigt"))

(defmethod trigger ((vnum integer))
;;logarithmic search
  (loop
     with top_of_trigt = (oneliner () () :int "top_of_trigt")
     with bot = 0
     with top = (1- top_of_trigt)
     with rnum = (floor (/ top_of_trigt 2))
     for trig = (make-instance 'trigger :rnum rnum)
     do
       (cond ((or (= rnum -1)
		  (= rnum top_of_trigt))
	      (error 'trigger-not-found))
	     ((= vnum (vnum trig))
	      (return trig))
	     ((< vnum (vnum trig))
	      (setf top (1- rnum)))
	     ((> vnum (vnum trig))
	      (setf bot (1+ rnum))))
       (setf rnum (floor (/ (+ top bot) 2)))))
