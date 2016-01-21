(with-vhdl ()
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
		  (concurrent-assign q (not a)))))
