;;(ffi:clines #.(format nil "#include \"~a\"" (asdf:system-relative-pathname :4d-lisp "../lisp-internal.h")))
(ffi:clines "#include \"../lisp-internal.h\"")
(in-package :4d)

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



(defclass room (entity)
  ((vnum :type :integer :reader vnum :initarg :vnum)))

(defmethod print-object ((room room) s)
  (format s "#<room ~d: ~a>" (vnum room) (title room)))

(defmacro room-field (room field type)
  `(oneliner ((vnum ,room))
	     (:int)
	     ,type
	     ,(format nil "world_vnum[#0]->~a" field)))

(defmethod pointer ((room room))
   (oneliner ((vnum room))
	     (:int)
	     :pointer-void
	     "world_vnum[#0]"))

;; properties of room
(defmethod title ((room room))
  (room-field room "name" :cstring))

(defmethod smell ((room room))
  (room-field room "smell" :cstring))

(defmethod sound ((room room))
  (room-field room "listen" :cstring))


(defmethod description ((room room))
  (4d-internal::room-get-description (pointer room)))



(defmethod characters ((room room))
  (loop with character = (oneliner ((vnum room)) (:int)
				       :pointer-void "world_vnum[#0]->people")
     until (ffi:null-pointer-p character)
     collect (ptr-to-character character)
     do (setf character (oneliner (character) (:pointer-void) :pointer-void "((Character*)#0)->next_in_room"))))

(defmethod objects ((room room))
  (loop with object = (oneliner ((vnum room)) (:int)
				       :pointer-void "world_vnum[#0]->contents")
     until (ffi:null-pointer-p object)
     collect (ptr-to-object-instance object)
     do (setf object (oneliner (object) (:pointer-void) :pointer-void "((struct obj_data*)#0)->next_content"))))

(defmethod contents ((room room))
  (objects room))

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

(defmethod triggers ((room room))
  (unless (ffi:null-pointer-p (oneliner ((vnum room)) (:int) :pointer-void
					"world_vnum[#0]->proto_script"))
    (loop for i from 0 to (1- (oneliner ((vnum room)) (:int) :int
					"world_vnum[#0]->proto_script->size()"))
       nconc (handler-case
		 (list (trigger (oneliner ((vnum room) i) (:int :int) :int
					  "world_vnum[#0]->proto_script->at(#1)")))
	       (trigger-not-found (e) (warn "trigger not found: ~d" (vnum e)) nil)))))


(defmethod script-pointer ((room room))
  (let ((ptr (oneliner ((vnum room)) (:int) :pointer-void
		       "SCRIPT(world_vnum[#0])")))
    (unless (ffi:null-pointer-p ptr)
      ptr)))

(defclass exit ()
  ((room :initarg :room)
   (direction :initarg :direction :reader direction)))

(defconstant +directions+ '(:north :east :south :west :up :down))

(defmethod exits ((room room))
  (let (result)
    (loop for dir in +directions+
	 nconc (alexandria:when-let ((exit (exit room dir)))
				       (list exit)))))
	    
(defmethod exit ((room room) direction)
  (let ((dir-num (position direction +directions+)))
    (unless dir-num
      (error "~s is not a valid direction" direction))
    
    (if (oneliner ((pointer room) dir-num) (:pointer-void :int) :bool
		  "((Room*)#0)->dir_option[#1] == NULL")
	nil
	(make-instance 'exit
		       :room room
		       :direction direction))))

(defmethod exit-from ((exit exit))
  (slot-value exit 'room))

(defmethod exit-to ((exit exit))
  (with-slots (room direction) exit
    (room
     (oneliner ((pointer room) (position direction +directions+))
	       (:pointer-void :int)
	       :int
	       "((Room*)#0)->dir_option[#1]->to_room->number"))))

(defmethod print-object ((exit exit) s)
  (format s "#<exit ~s to room ~d, ~a>"
	  (direction exit)
	  (vnum (exit-to exit))
	  (title (exit-to exit))))
