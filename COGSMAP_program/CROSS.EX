OPERATION CROSS

The CROSS operation creates an output overlay by combining the categories of
one input overlay with those of another input overlay on a point-by-point
basis, then assigning a user-specified value to all points of each
combination.  Combinations for which no output overlay value is specified are
automatically assigned an output overlay value of 0.

The general form of the instruction for this operation is:

       CR[OSS] overlay	 W[ITH] overlay   [F[OR] overlay]   [/]
       [A[SSIGNING] value   [T[O] value value]...   [TH[ROUGH] value]]...

where individual phrases are interpreted as follows.

       CROSS overlay   gives the title of the first input overlay to be
		       crossed.

       WITH overlay    gives the title of the second input overlay to be
		       crossed

       FOR overlay     gives the title of the output overlay. If omitted, the
		       title X is assumed.

       ASSIGNING value
		       gives an output value to be assigned.

       TO value value  gives the values from the first and second input
		       overlays, respectively, that define the points to which
		       the most recently specified output value is to be
		       assigned.

       THROUGH value   gives a value from the second input overlay which
		       effectively causes the most recently specified
		       TO-value-value phrase to be repeated for all values
		       greater than the one in that phrase and less than or
		       equal to this one.

       /	       indicates that the rest of the instruction is to be read
		       by skipping immediately to the next line of input text.
