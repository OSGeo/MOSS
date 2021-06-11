OPERATION DIVIDE

The DIVIDE operation creates an output overlay by dividing the values of one
input overlay by those of another input overlay on a point-by-point basis.  If
more than two input overlays are specified, each additional overlay is used to
further divide whatever output overlay values have been computed so far.  All
input overlays specified are processed in this manner according to the order in
which they appear in the DIVIDE instruction.

On output, all fractional values are truncated.  Division by 0 is permitted but
is processed such that any positive value divided by 0 results in an output
overlay value of -32767, and 0 divided by 0 results in a value of 0.

The general form of the instruction for this operation is:

       DIV[IDE] overlay   B[Y] overlay	 [B[Y] overlay]...   [/]
       [F[OR] overlay]

where individual phrases are interpreted as follows.

       DIVIDE overlay
		       gives the title of the initial input overlay of values
		       to be divided.

       BY overlay      gives the title of an additional input overlay of values
		       to be processed as divisors.

       FOR overlay     gives the title of the output overlay. If omitted, the
		       title X is assumed.

       /	       indicates that the rest of the instruction is to be read
		       by skipping immediately to the next line of input text.
