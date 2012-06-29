(ffi:clines #.(format nil "#include \"~a\"" (asdf:system-relative-pathname :4d-lisp "../lisp-internal.h")))
(in-package :4d)

(defclass zone ()
  ((znum :type :integer :reader znum :initarg :znum)
   (rnum :type :integer :reader rnum :initarg :rnum)))


(defmacro zone-field (zone field-name field-type)
  `(oneliner ((rnum ,zone)) (:int) ,field-type
	     ,(format nil "zone_table[#0].~a" field-name)))

(defmethod print-object ((zone zone) s)
  (format s "#<zone ~d: ~a>" (znum zone) (name zone)))

(defmethod name ((zone zone))
  (zone-field zone "name" :cstring))

(defmethod builders ((zone zone))
  (zone-field zone "builders" :cstring))

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



(defclass zone-reset ()
  ((zone :initarg :zone :reader zone-of)
   (num :initarg :num :reader reset-num)))

(defmacro reset-field (zone-reset field-name field-type)
  `(oneliner ((rnum (zone-of ,zone-reset)) (reset-num ,zone-reset)) (:int :int) ,field-type
	     ,(format nil "zone_table[#0].cmd[#1].~a" field-name)))

(defmethod dependent-on-last? ((zone-reset zone-reset))
  (reset-field zone-reset "if_flag" :bool))

(defmethod reset-arg1 ((zone-reset zone-reset))
  (reset-field zone-reset "arg1" :int))
(defmethod reset-arg2 ((zone-reset zone-reset))
  (reset-field zone-reset "arg2" :int))
(defmethod reset-arg3 ((zone-reset zone-reset))
  (reset-field zone-reset "arg3" :int))
(defmethod reset-arg4 ((zone-reset zone-reset))
  (reset-field zone-reset "arg4" :int))
(defmethod reset-line ((zone-reset zone-reset))
  (reset-field zone-reset "line" :int))
(defmethod reset-sarg1 ((zone-reset zone-reset))
  (reset-field zone-reset "sarg1" :cstring))
(defmethod reset-sarg2 ((zone-reset zone-reset))
  (reset-field zone-reset "sarg2" :cstring))

		 

(macrolet ((reset-types (&rest letter-types)
	     (let* ((resets 
		     (loop
			for lt in letter-types
			for letter = (car lt)
			for type = (cadr lt)
			collect `(,letter ,(intern (concatenate 'string "RESET-" (symbol-name type))))))
		    
		    (quoted-resets
		     (loop
			for lt in resets
			for letter = (first lt)
			for type = (second lt)
			collect `(,letter ',type))))
	       `(progn
		  ,@(loop for lt in resets
		       for type = (cadr lt)
		       collect `(defclass ,type (zone-reset) ()))
		  
		  (defun char-to-reset (c)
		    (case c
		      ,@quoted-resets))))))
  (reset-types
   (#\M mobile)
   (#\O object)
   (#\R remove-object)
   (#\T attach-trigger)
   (#\V set-variable)
   (#\P put)
   (#\* ignore)
   (#\G give)
   (#\E equip)
   (#\D door)
   (#\B bury)
   (#\Z maze)
   (#\S end)
   (t unknown)))

(defun zone-reset (zone num)
  (if (< num (oneliner ((rnum zone)) (:int) :int "zone_table[#0].cmd.size()"))
      (make-instance (char-to-reset (oneliner ((rnum zone) num) (:int :int) :char "zone_table[#0].cmd[#1].command"))
		     :zone zone
		     :num num)
      (error "no such reset.")))

(defun zone-resets (zone)
  (loop
     for i from 0 to (1- (zone-field zone "cmd.size()" :int))
     for reset = (zone-reset zone i)
     until (typep reset 'reset-end)
     collect reset))

(defmethod mobile-of (reset)
  (if (and (dependent-on-last? reset)
	   (> (reset-num reset) 0))
      (mobile-of (zone-reset (zone-of reset) (1- (reset-num reset))))
      nil))

(defmethod mobile-of ((reset reset-mobile))
  (mobile-prototype (reset-arg1 reset)))

(defmethod object-of (reset)
  (if (and (dependent-on-last? reset)
	   (> (reset-num reset) 0))
      (object-of (zone-reset (zone-of reset) (1- (reset-num reset))))
      nil))

(defmethod object-of ((reset reset-object))
  (object-prototype (obj-rnum-to-vnum (reset-arg1 reset))))
(defmethod object-of ((reset reset-remove-object))
  (object-prototype (obj-rnum-to-vnum (reset-arg2 reset))))
(defmethod object-of ((reset reset-put))
  (object-prototype (obj-rnum-to-vnum (reset-arg1 reset))))
(defmethod object-of ((reset reset-give))
  (object-prototype (obj-rnum-to-vnum (reset-arg1 reset))))
(defmethod object-of ((reset reset-equip))
  (object-prototype (obj-rnum-to-vnum (reset-arg1 reset))))

(defmethod room-of ((reset reset-mobile))
  (room (reset-arg3 reset)))
(defmethod room-of ((reset reset-object))
  (room (reset-arg3 reset)))
(defmethod room-of ((reset reset-attach-trigger))
  (if (= 0 (reset-arg3 reset))
      (call-next-method)
      (room (reset-arg3 reset))))
(defmethod room-of ((reset reset-set-variable))
  (room (reset-arg3 reset)))
(defmethod room-of ((reset reset-bury))
  (room (reset-arg3 reset)))

(defmethod room-of ((reset reset-door))
  (room (reset-arg1 reset)))
(defmethod room-of ((reset reset-remove-object))
  (room (reset-arg1 reset)))

(defmethod room-of (reset)
  (if (and (dependent-on-last? reset)
	   (> (reset-num reset) 0))
      (room-of (zone-reset (zone-of reset) (1- (reset-num reset))))
      nil))

(defmethod container-of ((reset reset-put))
  (object-prototype (obj-rnum-to-vnum (reset-arg3 reset))))

(defmethod trigger-of ((reset reset-attach-trigger))
  (make-instance 'trigger :rnum (reset-arg2 reset)))

(defmethod attach-type ((reset reset-attach-trigger))
  (case (reset-arg1 reset)
    (0 :mobile)
    (1 :object)
    (2 :room)))