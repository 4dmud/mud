;;unfortunately the current compile method does not allow automatic dependency loading.
;;therefore we load util.lisp upon detecting that a compile is happening.
;;this allows compilation at compile- and runtime
(eval-when (:compile-toplevel)
  (unless (boundp '*started*)
    (load "util.lisp")))

(in-package :4d)

(ffi:clines "#include \"lisp-internal.h\"")

(defclass zone ()
  ((znum :type :integer :reader znum :initarg :znum)
   (rnum :type :integer :reader rnum :initarg :rnum)))

(defmethod print-object ((zone zone) s)
  (format s "#<zone ~d: ~a>" (znum zone) (name zone)))

(defmethod name ((zone zone))
  (oneliner ((rnum zone)) (:int) :cstring "zone_table[#0].name"))

(defmethod builders ((zone zone))
  (oneliner ((rnum zone)) (:int) :cstring "zone_table[#0].builders"))

(defun zone (znum)
  (let ((rnum
	 (ffi:c-inline (znum) (:int) :int
"int rnum = -1;
 for (int i=0;i <= top_of_zone_table;i++) {
  if (zone_table[i].number == #0) {
    rnum = i;
    break;
  }
 }
 @(return)=rnum;")))
    (if (= -1 rnum)
	(error "no such zone.")
	(make-instance 'zone :znum znum :rnum rnum))))

(defun zones ()
  (loop with i = 0
     while (oneliner (i) (:int) :bool "#0 <= top_of_zone_table")
     collect (make-instance 'zone :rnum i :znum (oneliner (i) (:int) :int "zone_table[#0].number"))
     do (incf i)))



(defclass room ()
  ((vnum :type :integer :reader vnum :initarg :vnum)))

(defmethod print-object ((room room) s)
  (format s "#<room ~d: ~a>" (vnum room) (title room)))

(defmethod title ((room room))
  (oneliner ((vnum room)) (:int) :cstring "world_vnum[#0]->name"))

(defmethod zone-of ((room room))
  (declare (type room room))
  (let* ((rnum (oneliner ((vnum room)) (:int) :int
			 "world_vnum[#0]->zone"))
	 (znum (oneliner (rnum) (:int) :int
			 "zone_table[#0].number")))
    (make-instance 'zone :rnum rnum :znum znum)))
    

(defun room (vnum)
  (if (ffi:null-pointer-p (oneliner (vnum) (:int) :pointer-void
				    "world_vnum[#0]"))
      (error "no such room.")
      (make-instance 'room :vnum vnum)))

(defun rooms (zone)
  (declare (type zone zone))
  (let ((bottom (oneliner ((rnum zone)) (:int) :int
			   "zone_table[#0].bot"))
	 (top (oneliner ((rnum zone)) (:int) :int
			"zone_table[#0].top")))
   (loop for vnum from bottom to top nconc
	 (handler-case (list (room vnum))
	   (error () nil)))))

(defun all-rooms ()
  (let ((size (oneliner () () :int "world_vnum.size()")))
    (loop for vnum in 
	 (remove-if #'(lambda (vnum)
			(ffi:null-pointer-p (oneliner (vnum) (:int) :pointer-void "world_vnum[#0]")))
		    (loop for vnum from 0 to (1- size)
		       collect vnum))
	 collect (make-instance 'room :vnum vnum))))
