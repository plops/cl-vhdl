

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
;; components
;; Step 1. Generate the top-level entity declaration.
;; Step 2. Declare the lower-level design units used in design.
;; Step 3. Declare required internal signals used to connect the design units.
;; Step 4. Instantiate the design units.

(entity big-xnor
	((a b) :in std_logic)
	(f :out std_logic))

(architecture ckt1 big-xnor
	      (set f (not (or (and a (not b))
			      (andb (not a) b)))))

(entity big-and3
	((a b c) :in std_logic)
	(f :out std_logic))

(architecture ckt1 big-and3
	      (set f (and a b c)))

(entity my-compare ((a-in b-in) :in (std_logic_vector 2 :downto 0))
	(eq-out :out std_logic))

;; component declaration is placed after architecture line before begin
;; it is just the copy of entity. i think lisp should fill this out itself

;; architecture ckt1 of my_compare is
;; -- XNOR gate --------------------
;;   component big_xnor is
;;     Port ( A,B : in std_logic;
;;            F : out std_logic);
;;   end component;
;; -- 3-input AND gate -------------
;;   component big_and3 is
;;     Port ( A,B,C : in std_logic;
;;            F : out std_logic);
;;   end component;
;; -- intermediate signal declaration
;;   signal p1_out,p2_out,p3_out : std_logic;
;; begin
;;   x1: big_xnor port map (A => A_IN(2),
;;                          B => B_IN(2),
;;                          F => p1_out);
;;   x2: big_xnor port map (A => A_IN(1),
;;                          B => B_IN(1),
;;                          F => p2_out);
;;   x3: big_xnor port map (A => A_IN(0),
;;                          B => B_IN(0),
;;                          F => p3_out);
;;   a1: big_and3 port map (A
;;                          B
;;                          C
;;                          F
;;                          end ckt1;
;;                          =>
;;                          =>
;;                          =>
;;                          =>
;;                          p1_out,
;;                          p2_out,
;;                          p3_out,
;;                          EQ_OUT);
;; end ckt1;


(architecture ckt1 my-compare (big-xnor big-and3)
	      (let ((p1-out std_logic)
		    (p2-out std_logic)
		    (p3-out std_logic))
		(instantiate big-xnor
			     :label x1
			     :a (a-in 2)
			     :b (b-in 2)
			     :f p1-out)
		(instantiate big-xnor
			     :label x2
			     :a (a-in 1)
			     :b (b-in 1)
			     :f p2-out)
		(instantiate big-xnor
			     :label x3
			     :a (a-in 0)
			     :b (b-in 0)
			     :f p3-out)
		(instantiate big-and3
			     :label a1
			     :a p1-out
			     :b p2-out
			     :c p3-out
			     :f eq-out)))

;; implied mapping is possible but i think not as useful as the keywords

;; names of the signals can be the same

;; generic
;; entity gen_parity_check is
;;   generic ( n: positive);
;;   port    ( x: in std_logic_vector(n-1 downto 0);
;;             y: out std_logic);
;; end gen_parity_check;

(entity gen-parity-check
	:generic (n positive)
	(x :in (std_logic_vector (- n 1) :downto 0))
	(y :out std_logic))



;; architecture arch of gen_parity_check is
;; begin
;;   process(x)
;;     variable temp: std_logic;
;;   begin
;;     temp:='0';
;;     for i in x'range loop
;;       temp := temp XOR x(i);
;;     end loop;
;;     y <= temp;
;;   end process;
;; end arch;

;; note: i need array access here and also the function prime

(architecture arch gen-parity-check
	      (process (x)
		       (let ((temp std_logic))
			 (set temp 0)
			 (loop for i in (prime range x) do
			      (set temp (xor temp (aref x i))))
			 (set y temp))))


;; instantiation in another file:
;; architecture arch of my_parity_chk is
;; --------------- component declaration --------------------
;;   component gen_parity_check is
;;     generic ( std_logic : positive);
;;     port
;;       ( input
;;         : in std_logic_vector(N-1 downto 0);
;;         output
;;         : out std_logic);
;;   end component;
;; begin
;; -------------- component instantiation -------------------
;;   cp1: my_parity_chk generic map (4) port map (input, output);
;; end arch;

(architecture arch my-parity-chk  ;; i feel the architecture name should be autogenerated
	      ((gen-parity-check
		:generic positive))
	      ((input (- n 1)) output)
	      (instantiate :label cp1
			   :generic 4
			   (input output)))


;; i'm not sure if i should even implement generics. seems quite
;; complicated and is unnecessary when you have macros

;; you can include declarations using
;; library UNISIM;
;; use UNISIM.VComponents.all

;; register is a vector of d-flip flops on which all operations occur
;; simultaneously

;; i don't want to write std_logic all the time. perhaps this function
;; would be useful. it should expand to:
;; entity reg8 is
;;   Port ( REG_IN : in std_logic_vector(7 downto 0);
;;          LD,CLK : in std_logic;
;;          REG_OUT : out std_logic_vector(7 downto 0));
;; end reg8;

(entity* reg8
	 :in (reg-in 8) ld clk
	 :out (reg-out 8))

;; the following should expand to
;; architecture reg8-arch323 of reg8 is ...

(architecture* reg8
	       (process (clk)
			(when (and (event clk)
				   (= #b1 clk))
			  (set reg-out reg-in))))

;; 4 dataobjects: signal variable constant file

;; signal sig_name : sig_type [:= initial_value];

;; signals usually don't have initial value, they can't be implemented
;; in silicon but are useful for simulations

;; signal a : std_logic := '0';
;; signal i : integer;
;; signal b : integer range -128 to 127 := 0;
;; signal bus : std_logic_vector(3 downto 0) := "0011";

;; variable index_a : integer range (0 to 255) := 0;
;; constant max_cnt : integer := 12;

;; variables only in process and only for loops or as temporaries in
;; some calculation

;; a few standard types:
;; std_logic std_logic_vector boolean
;; boolean vector
;; integer (32 bit signed, -2,147,483,647 to 2,147,483,647)
;; natural (non-negative integer)
;; positive
;; integer_vector
;; character (256 symbols)
;; string (vector of character)

;; user-defined types:
;; type my_type is range 0 to 100;

;; custom datatype for 20 byte rom:

;; type memory is array (0 to 19 of std_logic_vector (7 downto 0));
;; constant my_rom : memory := (1 => "11111111"
;; 			     2 => ...  
;; 			     18 => ...
;; 			     others => ....);

;; vhdl synthesizer will do range checks

;; signed, unsigned from ieee.numeric_std
;; you should not use std_logic_arith (but this would allow to add std_logic_vector)
;; std_logic_vector is just a bag of bits and can not be used for numerical calculation

;; std_logic is resolved, multiple assignment to same signal is
;; possible without complaint from compiler

;; for loop

;; loop_label: for index in a_range loop
;;   sequential statements ..
;; end loop loop_label;

;; a_range examples:
;; 0 to 24
;; 24 downto 0

;; type of index variable is implicit
;; index variable can't be assigned to
;; only step 1


;; while loop
;; loop_label: while (condition) loop
;;   sequential stat ements ..
;; end loop loop_label;

;; more loop control statements:
;; next has two forms:

;; for i in 0 to 50 loop
;;   if (i = 20) then
;;     next;
;;   end if;
;;   i := i + 1;
;; end loop;

;; for i in 0 to 50 loop
;;   next when (i = 20);
;;   i := i + 1;
;; end loop;



;; exit , also two forms

;; standard digital circuits

;; mux
;; decoder
;; counter
;; comparator
;; register

(defcircuit d-flip-flop (clk d) (q)
	    (process (clk)
		     (when (and (event clk)
				(= #b1 clk))
		       (set q d))))

(defcircuit fet-d-flip-flop (clk d s) (q)
	    (process (clk)
		     (if (= s 0)
			 (set q 1)
			 (when (and (event clk)
				    (= 0 clk))
			   (set q d)))))

(defcircuit register-8-load-en (clk ld (d-in 8)) ((d-out 8))
	    (process (clk)
		     (when (and (event clk)
				(= #b1 clk))
		       (if (= ld 1)
			   (set d-out d-in)))))

(defcircuit counter-8b (reset clk ld up (d-in 8)) ((count 8))
	    (let ((t-cnt signal (unsigned 8)))
	     (process (clk reset)
		      (if (= reset 1)
			  (set t-cnt (others 0))
			  (when (and (event clk)
				     (= 1 clk))
			    (if (= ld 1)
				(set t-cnt (funcall unsigned d-in))
				(if (= up 1)
				    (set t-cnt (+ t-cnt 1))
				    (set t-cnt (- t-cnt 1)))))))
	     (set count (funcall std_logic_vector t-cnt))))

(defcircuit shift-register (clk d-in p-load (p-load-data 8))
  (process (clk)
	   (when (and (event clk)
		      (= 1 clk))
	     (if (= p-load 1)
		 (set reg-tmp p-load-data)
		 (set reg-tmp (& (reg-tmp 6 0) d-in))))
	   (set d-out (reg-tmp 7))))

(defcircuit comparator-8bit (clk (a-in 8) (b-in 8)) (alb agb aeb)
	    (process (clk)
		     (if (< a-in b-in)
			 (set alb 1)
			 (set alb 0))
		     (if (< b-in a-in)
			 (set agb 1)
			 (set agb 0))
		     (if (= a-in b-in)
			 (set aeb 1)
			 (set aeb 0))))
