;;search for all mob or object prototypes with a certain trigger
(eval-when (:compile-toplevel)
  (unless (boundp '*started*)
    (load "util.lisp")))

(in-package :4d)

(defcommand find (:min-level 52 :trust-groups (:sen))
  (handler-case 
      (let* ((stream (make-string-input-stream argument))
	     (type (read stream)))
	(case type
	  (resets (find-resets stream))
	  (trigger (find-trigger stream))
	  (t (error "no good."))))
    (error () (format t "usage: find find-type arguments~%Available finders: resets trigger~%"))))

(defun find-resets (stream)
    (handler-case
	(let ((type (read stream))
	      (vnum (read stream)))
	  (assert (typep type 'symbol))
	  (assert (typep vnum 'integer))

	  (case type
	    (mob (format t "Resets for mobile ~d, ~a:~%" vnum (name (mobile-prototype vnum))))
	    (obj (format t "Resets for object ~d, ~a:~%" vnum (short-description (object-prototype vnum))))
	    (t (error "no such type.")))
	  

	  (dolist (reset (case type
			   (mob (find-mobile-resets vnum))
			   (obj (find-object-resets vnum))
			   (t (error "no such type."))))
	    (let ((msg 
		   (typecase reset
		     (reset-object
		      (format nil "load object to room."))
		     (reset-mobile
		      (format nil "load mobile to room."))
		     (reset-put
		      (format nil "put in object ~d, ~a" (vnum (container-of reset)) (short-description (container-of reset))))
		     (reset-remove-object
		      (format nil "remove object."))
		     (reset-give
		      (format nil "give object to ~d, ~a" (vnum (mobile-of reset)) (name (mobile-of reset))))
		     (reset-equip
		      (format nil "equip object on ~d, ~a" (vnum (mobile-of reset)) (name (mobile-of reset)))))))
	      (if msg
		  (format t "in room ~d, ~a: ~a~%" (vnum (room-of reset)) (title (room-of reset)) msg)))))
      (object-prototype-not-found (e) (format t "No object found with vnum ~d.~%" (vnum e)))
      (mobile-prototype-not-found (e) (format t "No mobile found with vnum ~d.~%" (vnum e)))
      (error () (format t "usage: find resets [obj|mob] vnum.~%"))))
	
(defun find-object-resets (vnum)
  (delete-if-not #'(lambda (reset)
		     (handler-case 
			 (and (object-of reset)
			      (eql vnum (vnum (object-of reset))))
		       (error () nil)))
		     (mapcan #'zone-resets (zones))))

(defun find-mobile-resets (vnum)
  (delete-if-not #'(lambda (reset)
		     (handler-case
			 (and (mobile-of reset)
			      (eql vnum (vnum (mobile-of reset)))
			      (typep reset 'reset-mobile)) ;;only mobile type
		       (error () nil)))
		 (mapcan #'zone-resets (zones))))

(defun find-trigger (stream)
  (handler-case
      (let ((vnum (read stream)))
	(assert (typep vnum 'integer))

	(format t "Trigger ~d, ~a is attached to the following entities:~%"
		vnum (name (trigger vnum)))

	(dolist (item (delete-if-not #'(lambda (item)
					 (member vnum (mapcar #'vnum (triggers item))))
				     (nconc (all-rooms) (all-mobile-prototypes) (all-object-prototypes))))
	  (typecase item
	    (room (format t "Room ~d, ~a.~%" (vnum item) (title item)))
	    (mobile-prototype (format t "Mobile ~d, ~a.~%" (vnum item) (name item)))
	    (object-prototype (format t "Object ~d, ~a.~%" (vnum item) (short-description item)))))
	(dolist (reset (delete-if-not #'(lambda (reset)
					  (and 
					   (typep reset 'reset-attach-trigger)
					   (handler-case 
					       (= vnum (vnum (trigger-of reset)))
					     (trigger-not-found () nil))))
				      (mapcan #'zone-resets (zones))))
	  (format t "In room ~d, ~a: "
		  (vnum (room-of reset)
			(title (room-of reset))))
	  (case (attach-type reset)
	    (:object
	      (format t "Attached to object ~d, ~a by zone reset.~%"
		      (vnum (object-of reset))
		      (short-description (object-of reset))))
	    (:mobile
	      (format t "Attached to mobile ~d, ~a by zone reset.~%"
		      (vnum (mobile-of reset))
		      (name (mobile-of reset)))))))
    (trigger-not-found (e) (format t "No trigger with vnum ~d.~%" (vnum e)))
    (error (e) (format t "usage: find trigger vnum~%~a~%" e))))
		     