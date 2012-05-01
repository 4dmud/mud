;;;code to inspect the clans in funny ways
(defpackage :4d
  (:use :cl))
(in-package :4d)

(eval-when (:compile-toplevel)
  (unless (boundp '*started*)
    (load "util.lisp")))

(ffi:clines "#include \"lisp-internal.h\"")

(defclass clan ()
  ((id :initarg :id :reader id)))

(defmethod print-object ((clan clan) s)
  (format s "#<clan ~a>" (name clan)))

(defmethod name ((clan clan))
  (oneliner ((id clan)) (:int) :cstring "clan[#0].name"))

(defmethod ranks ((clan clan))
  (oneliner ((id clan)) (:int) :int "clan[#0].ranks"))


(defmethod clan ((id integer))
  (if (oneliner (id) (:int) :bool "#0 < num_of_clans")
      (make-instance 'clan :id id)
      (error "No such clan.")))

(defun clans ()
  (loop for id from 0 to (1- (oneliner () () :int "num_of_clans"))
       collect (make-instance 'clan :id id)))

(defmethod clan ((name string))
  (or (loop for clan in (clans) do
	   (if (string= (string-upcase (name clan))
			(string-upcase name))
	       (return clan)))
      (error "No such clan.")))

(defclass deed ()
  ((zone :type :integer :initarg :zone-id)
   (clan :type clan :initarg :clan :reader clan)
   (claimer :type :string :initarg :claimer :reader claimed-by)))

(defmethod zone-of ((deed deed))
  (zone (slot-value deed 'zone)))

(defmethod print-object ((deed deed) s)
  (format s "#<deed ~a, claimed by ~a>" (name (zone-of deed)) (claimed-by deed)))

(defmethod deeds ((clan clan))
  (loop with deed = (oneliner ((id clan)) (:int) :pointer-void
			      "clan[#0].deeds")
     until (ffi:null-pointer-p deed)
     collect (make-instance 'deed
			    :zone-id (oneliner (deed) (:pointer-void) :int
					       "((clan_deed_type*)#0)->zone")
			    :clan clan
			    :claimer (oneliner (deed) (:pointer-void) :cstring
					       "((clan_deed_type*)#0)->name"))
     do (setf deed (oneliner (deed) (:pointer-void) :pointer-void
			     "((clan_deed_type*)#0)->next"))))

(defun all-deeds ()
  (mapcan #'deeds (clans)))

(defun set-gold-tokens (clan amount)
  (ffi:c-inline ((id clan) amount) (:int :int) :void
		"clan[#0].treasury.gold = #1;" :side-effects t))