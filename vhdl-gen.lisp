(defpackage :vhdl
  (:use :cl))

(in-package :vhdl)

(setf (readtable-case *readtable*) :invert)

(defmacro f (fmt &rest rest)
  "abbreviation for format nil"
  `(format nil ,fmt ,@rest))

(defmacro c (&body body)
  "combine multiple outputs into string"
  `(with-output-to-string (s)
     (macrolet ((f (fmt &rest rest)
		  `(format s ,fmt ,@rest)))
       ,@body)))

(defun emit (code)
  (cond
    ((null code) "")
    ((listp code)
     (case (car code)
       (entity (destructuring-bind (name &key ports) (cdr code)
		 (c (f "entity ~a is~%" name)
		    (f "  port(~%~{~a~^~%~});~%" ports)
		    (f "end ~a;" name))))
       (t (cond ((and (= 2 (length code)) 
		      (member (car code) '(-))) ;; unary operators
		 (destructuring-bind (op operand) code
		   (f "(~a (~a))" op (emit operand))))))))))

(emit `(entity ckt_e :ports ((ram-cs))))
