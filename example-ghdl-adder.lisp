(in-package :vhdl)
(emit `(architecture rtl adder
		     (assign s (xor i0 i1 ci))
		     (assign co (or (and i0 i1)
				    (and i0 ci)
				    (and i1 ci)))))
