;;;utils.lisp: utility macros necessary at compile time
(defpackage :4d
  (:use :cl)
  (:shadow room))
(in-package :4d)
(defmacro oneliner (&rest args)
  `(ffi:c-inline ,@args :side-effects nil :one-liner t))