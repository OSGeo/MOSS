OPERATION SIZE

The SIZE operation creates an output overlay by assigning a new value to each
region of an input overlay indicating the number of points in that region.

The general form of the instruction for this operation is:

       SI[ZE] overlay	[F[OR] overlay]

where individual phrases are interpreted as follows.

       SIZE overlay    gives the title of the input overlay.

       FOR overlay     gives the title of the output overlay. If omitted, the
		       title X is assumed.
