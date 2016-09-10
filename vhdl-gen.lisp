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
       (entity (destructuring-bind (identitfier header &key (declarative) (simple-name)) (cdr code)))
       (t (cond ((and (= 2 (length code)) 
		      (member (car code) '(-))) ;; unary operators
		 (destructuring-bind (op operand) code
		   (f "(~a (~a))" op (emit operand))))))))))

;; backus naur for vhdl
;; http://www.pldworld.net/_hdl/1/www.ireste.fr/fdl/vcl/lesd/Vbnf.htm
