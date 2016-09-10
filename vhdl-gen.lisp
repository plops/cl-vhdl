(defpackage :vhdl-generator
  (:use :cl))

(in-package :vhdl-generator)

(setf (readtable-case *readtable*) :invert)

(defun emit-vhdl (code)
  (cond
    ((null code) "")
    (())))
