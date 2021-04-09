OPERATION COVER

The COVER operation creates an output overlay by reproducing the values of one
input overlay, then replacing or "covering" these values with the values of
another input overlay on all points where this second overlay value is anything
other than 0.  If more than two input overlays are specified, each is used to
cover whatever values have already been generated.  All input overlays
specified as processed in this manner according to the order in whch they
appear in the COVER instruction.

The general form of the instruction for this operation is:

       COV[ER] overlay	 W[ITH] overlay   [W[ITH] overlay]...	[/]
       [F[OR] overlay]

where individual phrases are interpreted as follows.

       COVER overlay   gives the title of the input overlay to be covered.

       WITH overlay    gives the title of an additional input overlay to cover
		       any that have already been specified.

       FOR overlay     gives the title of the output overlay. If omitted, the
		       title X is assumed.

       /	       indicates that the rest of the instruction is to be read
		       by skipping immediately to the next line of input text.
