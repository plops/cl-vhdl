

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

;; selected assignment
;; with <choose_expression> select
;;      target <= <expression> when <choices>,
;;                <expression> when <choices>;
;; architecture f3_4 of my_ckt_f3 is
;; begin
;;   with ((L ='0' and M ='0'and N ='1')or(L='1' and M='1')) select
;;     F3 <= '1' when '1',
;;     '0' when '0',
;;     '0' when others;
;; end f3_4;

;; ideally i only want set instead of select-assign, conditional-assign, concurrent-assign and so on
(set f3 (case (or (and (= l #b0) (m #b0) (n #b1))
		   (and (= l #b1) (m #b1)))
	  (#b1 #b1)
	  (#b0 #b0)
	  (t #b0)))

;;;; process statement

;; the label is optional, should be included for self description
;; don't use complex processes, keep them simple
;; then the synthesizer will be more efficient

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

;; this only in process body:
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

;; case statement
;; sequential equivalent of the selected assignment
;; when others should be used as good practice
;; case (expression) is
;;   when choices =>
;;     <sequential statements>
;;   when choices =>
;;     <sequential statements>
;;   when others =>
;;     <sequential statements>
;; end case;

(case expression
  (choice1 statement))


;; operators

exponential=**
abs

and or nand nor xor xnor not

* / mod rem 

+ -

concatenate=&
logic shift: sll srl        -> lsh +,-  introduces zeros ieee.numeric_std, .._bit
arithmethic shift: sla sra  -> ash +,- sign bit unchanged
rotate: rol ror             -> rot 

relational = /= < <= > >=

(mod rem and & only on some types)

;;;; d flip flop

;; library IEEE;
;; use IEEE.std_logic_1164.all;
;; -- entity
;; entity d_ff is
;;   port ( D, CLK : in std_logic;
;;          Q :
;;          out std_logic);
;; end d_ff;
;; -- architecture
;; architecture my_d_ff of d_ff is
;; begin
;;   dff: process(CLK)
;;   begin
;;     if (rising_edge(CLK))
;;     then
;; --or if (CLK'event and CLK='1') then
;;       Q <= D;
;;     end if;
;;   end process dff;
;; end my_d_ff;

(entity d-ff (((d clk) :in std_logic)
	      (q :out std_logic)))
(architecture my-d-ff d-ff
	      (process (clk)
		       (when (and (event clk)
				  (= #b1 clk))
			 (set q d))))

;; vhdl doesn't change the output. because there is no else in the if,
;; the implementation has to remember the previous value.

;; in order to check if you use memory check for the presence of an
;; else clause in an if statement. then the synthesizer will generate
;; a latch

;; synchronous input S that sets the d flip flop when asserted
(process (clk)
	 (when (and (event clk)
		    (= #b1 clk))
	   (if (= s 0)
	       (set q 1)
	       (set q d))))

;; asynchronous input R that resets the flip flop
(process (clk)
	 (cond ((= r 1) (set q 0))
	       ((and (event clk) (= 1 clk)) (set q d))))

;; t flip flop output depends on its current output.  use an
;; intermediate signal to get to the output signal.  the intermediate
;; signal may stand on both sides of the assignment operator

;; t flip flops (toggle) have certain advantages over d flip flops

;; when t is high, clock frequency divided by 2

(entity t-ff-s (((tin s clk) :in std_logic)
		(q :out std_logic)))
(architecture
 my-tff-s tff-s
 (process (s clk)
	  (let ((tmp std_logic))
	    (cond ((= s 0) (set tmp 1))
		  ((and (event clk) (= 1 clk)) (set tmp (xor tin tmp))))))
 (set q tmp))


;; finite state machines

;; split fsm into two processes. a synchronous one for clocking and
;; storage and a combinatorial one for next state and output decoder

(entity fsm1-entity
	(tog-en :in std_logic)
	((clk clr) :in std_logic)
	(z1 :out std_logic))

(architecture
 fsm1 fsm1-entity
 (type ((state-type (st0 st1)))
       (let ((ps state-type)
	     (ns state-type))
	 (process sync-proc (clk ns clr)
		  ;; storage only of ps
		  (cond ((= clr 1) (set ps st0))
			((and (event clk) (= 1 clk))
			 (set ps ns))))
	 (process comb-proc (ps tog-en)
		  (set z1 0) ;; preassignment prevents latches
		  (case ps
		    (st0 (set z1 0)
			 (if (= tog-en 1)
			     (set ns st1)
			     (set ns st0)))
		    (st1 (set z1 1)
			 (if (= tog-en 1)
			     (set ns st0)
			     (set ns st1)))
		    (t (set z1 0)
		       (set ns st0)))))))

;; force the encoding of the states

(architecture
 fsm1 fsm1-entity
 (type ((state-type (st0 st1)))
       (attribute ((enum-encoding string)
		   (enum-encoding state-type "1000 0100 0010 0001"))
	(let ((ps state-type)
	      (ns state-type))
	  (process sync-proc (clk ns clr)
		   ;; storage only of ps (previous state)
		   (cond ((= clr 1) (set ps st0))
			 ((and (event clk) (= 1 clk))
			  (set ps ns))))
	  (process comb-proc (ps tog-en)
		   (set z1 0) ;; preassignment prevents latches
		   (case ps
		     (st0 (set z1 0)
			  (if (= tog-en 1)
			      (set ns st1)
			      (set ns st0)))
		     (st1 (set z1 1)
			  (if (= tog-en 1)
			      (set ns st0)
			      (set ns st1)))
		     (t (set z1 0)
			(set ns st0))))))))
