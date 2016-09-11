(defpackage :vhdl
  (:use :cl))

(in-package :vhdl)

(setf (readtable-case *readtable*) :invert)

(defmacro f (fmt &rest rest)
  "abbreviation for format nil"
  `(format nil ,fmt ,@rest))

(let ((fmt "~a ~a")
      (rest '( 1 2 )))
 `(progn
    (list 'format 3 'fmt 'rest)))

(defmacro ft (fmd &rest rest)
  `(format t ,fmd ,@rest))

(defmacro c (&body body)
  "combine multiple outputs into string, overload standard output so that you can use fs as a abbreviation to (format t ...)"
  (let ((stream (intern (format nil "~a" (gensym "STREAM")))))
    `(with-output-to-string (,stream)
       (let ((*standard-output* ,stream))
	,@body))))

#+nil
(c (ft "bla ~a ~a" 3 3)
   (ft "blub"))

(defun print-type (type)
  (cond
    ((listp type) (destructuring-bind (name start &optional (end 0)) type
		      (f "~a( ~a ~a ~a )" name start (if (< start end)
						'to
						'downto)
			 end)))
    (t (f "~a" type))))

(format nil "~{~a=~a ~}" '((a 2) (3 a)))



(defun emit (code)
  (cond
    ((null code) "")
    ((listp code)
     (case (car code)
       ;(architecture )
       (entity (destructuring-bind (name &key ports) (cdr code)
		 (c (ft "entity ~a is~%" name)
		    (ft "  port(~%~{    ~a~^;~%~});~%"
			(loop for (name dir type) in ports collect
			     (f "~a : ~a ~a" name dir (print-type type))))
		    (ft "end ~a;" name))))
       (cond-assign (destructuring-bind (target &rest clauses) (cdr code)
		      (c (ft "~a <= ~%" target)
			 (loop for (condition expression) in clauses do
			      (if (eql condition 't)
				  (ft "  (~a);~%" expression condition)
				  (ft "  (~a) when (~a) else~%" expression condition))))))
       (t (cond ((and (= 2 (length code)) 
		      (member (car code) '(-))) ;; unary operators
		 (destructuring-bind (op operand) code
		   (f "(~a (~a))" op (emit operand))))))))))
#+nil
(emit `(cond-assign target (cond1 exp1) (cond2 exp2) (t exp3)))

;; use slime-eval-print-last expression to get these outputs

"target <= 
  (exp1) when (cond1) else
  (exp2) when (cond2) else
  (exp3);
"

#+nil

(emit `(entity ckt_e :ports ((ram_cs :in std_logic)
			     (ram_we :in std_logic)
			     (ram_we :in std_logic)
			     (sel_op1 :in (std_logic_vector 3)))))
"entity ckt_e is
  port(
    ram_cs : in std_logic;
    ram_we : in std_logic;
    ram_we : in std_logic;
    sel_op1 : in std_logic_vector( 3 downto 0 ));
end ckt_e;"
