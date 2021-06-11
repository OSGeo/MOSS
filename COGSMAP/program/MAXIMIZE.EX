OPERATION MAXIMIZE

The MAXIMIZE operation creates an output overlay by comparing the values of two
or more input overlays on a point-by-point basis, then assigning to each point
the largest of its input values.

The general form of the instruction for this operation is:

       MAX[IMIZE] overlay   V[ERSUS] overlay   [V[ERSUS] overlay]...   [/]
       [F[OR] overlay]

where individual phrases are interpreted as follows.

       MAXIMIZE overlay
		       gives the title of one of the input overlays to be
		       maximized.

       VERSUS overlay
		       gives the title of an additional input overlay to be
		       maximized.

       FOR overlay     gives the title of the output overlay. If omitted, the
		       title X is assumed.

       /	       indicates that the rest of the instruction is to be read
		       by skipping immediately to the next line of input text.
