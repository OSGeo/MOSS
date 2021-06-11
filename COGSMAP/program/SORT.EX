OPERATION SORT

The SORT operation creates an output overlay by replacing the lowest value on
an input overlay with an output value of 1, the second lowest with a value of
2, the third with a 3, and so on.

The general form of the instruction for this operation is:

       SO[RT] overlay	[F[OR] overlay]

where individual phrases are interpreted as follows.

       SORT overlay    gives the title of the input overlay.

       FOR overlay     gives the title of the output overlay. If omitted, the
		       title X is assumed.
