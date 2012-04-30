;;search for all mob or object prototypes with a certain trigger
(eval-when (:compile-toplevel)
  (unless (boundp '*started*)
    (load "util.lisp")))

(in-package :4d)

(defcommand find-resets (:min-level 52 :trust-groups (:sen))
    (handler-case
	(let* ((stream (make-string-input-stream argument))
	       (type (read stream))
	       (vnum (read stream)))
	  (assert (typep type 'symbol))
	  (assert (typep vnum 'integer))

	  (case type
	    (mobile (format t "Resets for mobile ~d, ~a:~%" vnum (name (mobile-prototype vnum))))
	    (object (format t "Resets for object ~d, ~a:~%" vnum (short-description (object-prototype vnum))))
	    (t (error "no such type.")))
	  

	  (dolist (reset (case type
			   (mobile (find-mobile-resets vnum))
			   (object (find-object-resets vnum))
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
      (error () (format t "usage: find-resets [object|mobile] vnum.~%"))))


(defun find-object-resets (vnum)
  (remove-if-not #'(lambda (reset)
		     (handler-case 
			 (and (object-of reset)
			      (eql vnum (vnum (object-of reset))))
		       (error () nil)))
		     (mapcan #'zone-resets (zones))))

(defun find-mobile-resets (vnum)
  (remove-if-not #'(lambda (reset)
		     (handler-case
			 (and (mobile-of reset)
			      (eql vnum (vnum (mobile-of reset)))
			      (typep reset 'reset-mobile)) ;;only mobile type
		       (error () nil)))
		 (mapcan #'zone-resets (zones))))
