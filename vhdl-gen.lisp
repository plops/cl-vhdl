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
     (macrolet ((fs (fmt &rest rest)
		  `(format s ,fmt ,@rest)))
       ,@body)))

(defun print-type (type)
  (cond
    ((listp type) (destructuring-bind (name start &optional (end 0)) type
		      (f "~a( ~a ~a ~a )" name start (if (< start end)
						'to
						'downto)
			 end)))
    (t (f "~a" type))))

(defun emit (code)
  (cond
    ((null code) "")
    ((listp code)
     (case (car code)
       (entity (destructuring-bind (name &key ports) (cdr code)
		 (c (fs "entity ~a is~%" name)
		    (fs "  port(~%~{    ~a~^;~%~});~%" (loop for (name dir type) in ports collect
						       (f "~a : ~a ~a" name dir (print-type type))))
		    (fs "end ~a;" name))))
       (t (cond ((and (= 2 (length code)) 
		      (member (car code) '(-))) ;; unary operators
		 (destructuring-bind (op operand) code
		   (f "(~a (~a))" op (emit operand))))))))))

(emit `(entity ckt_e :ports ((ram_cs :in std_logic)
			     (ram_we :in std_logic)
			     (ram_we :in std_logic)
			     (sel_op1 :in (std_logic_vector 3)))))
