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
    ((atom code) (f "~a" code))
    ((listp code)
     (case (car code)
       (block (c (ft "begin~%")
		 (ft "~{  ~a;~}" (mapcar #'emit (cdr code)))
		 (ft "end~%")))
       (architecture (destructuring-bind (architecture-name entity-identifier &rest rest) (cdr code)
		       (c (ft "architecture ~a of ~a is~%" architecture-name entity-identifier)
			  (ft "~a" (emit `(block ,@rest))))))
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
				  (ft "  (~a)~%" (emit expression))
				  (ft "  (~a) when (~a) else~%" (emit expression) (emit condition)))))))
       (t (cond ((and (= 2 (length code)) 
		      (member (car code) '(-))) ;; unary operators
		 (destructuring-bind (op operand) code
		   (f "(~a (~a))" op (emit operand))))
		((member (car code) '(or and))
		 (destructuring-bind (op &rest args) code
		   (c (ft "( ")
		      (loop for e in (butlast args) do
			   (ft "~a ~a " (emit e) op))
		      (ft "~a )" (emit (car (last args)))))))))))))

(defun lev-slow (a b)
  (declare (optimize (speed 0) (safety 3) (debug 3))
	   (type simple-string a b)
	   (values fixnum &optional))
  (labels ((frob (a b i j)
	     (declare (type fixnum i j)
		      (type simple-string a b)
		      (values fixnum &optional))
	     (if (= 0 i) (return-from frob j))
	     (if (= 0 j) (return-from frob i))
	     (let ((cost (if (eq (aref a (- i 1))
				 (aref b (- j 1)))
			     0
			     1)))
	       (min (+ 1 (frob a b (- i 1) j))
		    (+ 1 (frob a b i (- j 1)))
		    (+ (frob a b (- i 1) (- j 1))
		       cost)))))
    (frob a b (length a) (length b))))

(defun lev (a b)
  (declare (optimize (speed 0) (safety 3) (debug 3))
	   (type simple-string a b)
	   (values fixnum &optional))
  (cond
    ;; degenerate cases
    ((string= a b) 0)
    ((= 0 (length a)) (length b))
    ((= 0 (length b)) (length a))
    (t ;; create two work vectors with integer distance
     (let ((v0 ;; previous row of distances, for an empty a. this
	       ;; distance is the number of characters to delete from
	       ;; b
	    (make-array (+ 1 (length b)) :element-type 'fixnum))
	   (v1 (make-array (+ 1 (length b)) :element-type 'fixnum)))
       (declare (type (simple-array fixnum 1) v0 v1))
       (dotimes (i (length v0))
	 (setf (aref v0 i) i))
       (dotimes (i (length a))
	 ;; calculate current row distances v1
	 ;; delete i+1 chars from a to match empty b
	 (setf (aref v1 0) (+ i 1))
	 (dotimes (j (length b)) ;; fill the rest of the row
	   (setf (aref v1 (+ j 1)) (min (1+ (aref v1 j))
					(1+ (aref v0 (+ j 1)))
					(+ (aref v0 j)
					   (if (eq (aref a i)
						   (aref b j))
					       0
					       1)))))
	 
	 (dotimes (j (length v0))
	   ;; copy current row (v1) to previous row for next iteration
	   (setf (aref v0 j) (aref v1 j))))
       (aref v1 (length b))))))

#+nil
(lev "GUMBO" "GAMBOL")


(defun test (a b)
  (lev (emit a) b))


(emit `(architecture f3_4 my_ckt_f3 (assign target (cond1 3) (t 2))))


(emit `(cond-assign target ((or a (and c b)) exp1) (cond2 exp2) (t exp3)))

;; use slime-eval-print-last expression to get these outputs
(time (test
  `(cond-assign target (cond1 exp1) (cond2 exp2) (t exp3))
  "target <= 
  (exp1) when (cond1) else
  (exp2) when (cond2) else
  (exp3)
"))

(test
 `(entity ckt_e
	  :ports ((ram_cs :in std_logic)
			(ram_we :in std_logic)
			(ram_we :in std_logic)
			(sel_op1 :in (std_logic_vector 3))))
 "entity ckt_e is
  port(
    ram_cs : in std_logic;
    ram_we : in std_logic;
    ram_we : in std_logic;
    sel_op1 : in std_logic_vector( 3 downto 0 ));
end ckt_e;")
