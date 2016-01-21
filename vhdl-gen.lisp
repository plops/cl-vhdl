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
				    (set va 74
					 vb 67)
				    (sequential-assign a1 (and a b c))
				    (if (< vb va)
					(assign f (or a1 b)))))
		  (comment "we are outside the process body")
		  (concurrent-assign q (not a)))))
