OPERATION SUBTRACT

The SUBTRACT operation creates an output overlay by subtracting the values of
one input overlay from those of another input overlay on a point-by-point
basis.	If more than two input overlays are specified, the values of each
additional overlay are subtracted from whatever output overlay values have been
computed so far.  All input overlays specified are processed in this manner
according to the order in which they appear in the SUBTRACT instruction.

The general form of the instruction for this operation is:

       SUB[TRACT] overlay   F[ROM] overlay   [F[ROM] overlay]...   [/]
       [F[OR] overlay]

where individual phrases are interpreted as follows.

       SUBTRACT overlay
		       gives the title of the initial input overlay of values
		       from which others are to be subtracted.

       FROM overlay    gives the title of an additional input overlay of values
		       to be subtracted.

       FOR overlay     gives the title of the output overlay. If omitted, the
		       title X is assumed.

       /	       indicates that the rest of the instruction is to be read
		       by skipping immediately to the next line of input text.
