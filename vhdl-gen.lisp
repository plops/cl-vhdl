;; -- library declaration
;; library IEEE;
;; use IEEE.std_logic_1164.all;
;; -- entity
;; entity my_system is
;;   port ( A,B,C : in std_logic;
;;          F,Q
;;          : out std_logic);
;; end my_system;
;; -- architecture
;; architecture behav of my_system is
;;   signal A1 : std_logic;
;; begin
;;   some_proc: process(A,B,C) is
;;     variable a,b : integer;
;;   begin
;;     a:=74;
;;     b:=67;
;;     A1 <= A and B and C;
;;     if a>b then
;;       F <= A1 or B;
;;     end if;
;;   end process some_proc;
;; -- we are outside the process body
;;   Q <= not A;
;; end behav;

(with-vhdl ()
  ;; example listing 5.4 in free range vhdl
  (library IEEE)
  (use IEEE.std_logic_1164.all)
  (entity my-system
	  (port
	   (a :in std_logic)
	   (b :in std_logic)
	   (c :in std_logic)
	   (f :out std_logic)
	   (q :out std_logic))
	  )
  (architecture behav my-system
		(signal a1 std_logic)
		(progn
		  (process some-proc (a b c)
			   (let-var ((va integer)
				     (vb integer))
				    (variable-assign va 74)
				    (variable-assign vb 67)
				    (sequential-assign a1 (and a b c))
				    (if (< vb va)
					(assign f (or a1 b)))))
		  (comment "we are outside the process body")
		  (concurrent-assign q (not a)))))

;; Listing 5.5: Syntax of the if statement.
;; if (condition1) then
;;   <statements1>
;; elsif (condition2) then
;;   <statements2>
;; else
;;   <statements3>
;; end if;

;; sequential equivalent of conditional signal assigment
;; final else clause is optional, has deep ramifications

(with-vhdl ()
 (cond ((condition1 statements1)
	(condition2 statements2)
	(t statements3))))


;; entity ckt_e is
;;   port(
;;     RAM_CS, RAM_WE, RAM_OE  : in std_logic;
;;     SEL_OP1, SEL_OP2 : in std_logic_vector(3 downto 0);
;;     RAM_DATA_IN : in std_logic_vector(9 downto 0);
;;     RAM_DATA_OUT : in std_logic_vector(7 downto 0));
;; end ckt_e;              

(with-vhdl ()
  (entity ckt_e
	  ((ram-cs ram-we ram-oe) :in std_logic)
	  ((sel-op1 sel-op2) :in (std_logic_vector 3 :downto 0))
	  (ram-data-in :in (std_logic_vector 9 :downto 0))
	  (ram-data-out :in (std_logic_vector 7 :downto 0))))

;; conditional assignment
;; <target> <= <expression1> when <condition1> else
;;             <expression2> when <condition2> else
;;             <expression3>;

(assign target
	(cond ((condition1 expression1))
	      ((condition2 expression2))
	      (t expression3)))
