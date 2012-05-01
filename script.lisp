(eval-when (:compile-toplevel)
  (unless (boundp '*started*)
    (load "util.lisp")))
(ffi:clines "#include \"lisp-internal.h\"")

(defclass script ()
  ((rnum :type :integer :initarg :vnum :reader rnum)))

