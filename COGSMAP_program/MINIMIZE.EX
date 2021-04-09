OPERATION MINIMIZE

The MINIMIZE operation creates an output overlay by comparing the values of two
or more input overlays on a point-by-point basis, then assigning to each point
the smallest of its input values.

The general form of the instruction for this operation is:

       MIN[IMIZE] overlay   V[ERSUS] overlay   [V[ERSUS] overlay]...   [/]
       [F[OR] overlay]

where individual phrases are interpreted as follows.

       MINIMIZE overlay
		       gives the title of one of the input overlays to be
		       minimized.

       VERSUS overlay
		       gives the title of an additional input overlay to be
		       minimized.

       FOR overlay     gives the title of the output overlay. If omitted, the
		       title X is assumed.

       /	       indicates that the rest of the instruction is to be read
		       by skipping immediately to the next line of input text.
