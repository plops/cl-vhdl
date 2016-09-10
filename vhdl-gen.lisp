(defpackage :vhdl
  (:use :cl))

(in-package :vhdl)

(setf (readtable-case *readtable*) :invert)

(defmacro f (fmt &rest rest)
  `(format nil ,fmt ,@rest))

(defun emit (code)
  (cond
    ((null code) "")
    ((listp code)
     (case (car code)
       (t (cond ((and (= 2 (length code)) 
		      (member (car code) '(-))) ;; unary operators
		 (destructuring-bind (op operand) code
		   (f "(~a (~a))" op (emit operand))))))))))
