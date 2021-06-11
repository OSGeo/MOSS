OPERATION ORIENT

The ORIENT operation creates an output overlay by computing aspects at points
located on a three-dimensional surface. Aspect is the compass direction toward
which such a surface slopes downward. The third dimensional position of each
point on this surface is defined by its value on an input overlay.

Output values may be computed over a range of 0 through 8 such that:

       0 represents non-sloping areas;
       1 represents slopes facing the top edge of a map;
       2 represents slopes facing the upper right corner of a map;
       3 represents slopes facing the right edge of a map;
       4 represents slopes facing the lower right corner of a map;
       5 represents slopes facing the bottom edge of a map;
       6 represents slopes facing the lower left corner of a map;
       7 represents slopes facing the left edge of a map; and
       8 represents slopes facing the upper left edge of a map

or over a range of 0 through 360 such that, for example:

	 0 represents non-sloping areas;
	45 represents slopes facing the upper right edge of a map;
	90 represents slopes facing the right edge of a map;
       180 represents slopes facing the bottom edge of a map;
       360 represents slopes facing the top edge of a map; and so on.

The general form of the instruction for this operation is:

       OR[IENT] overlay   [P[RECISELY]]   [F[OR] overlay]

where individual phrases are interpreted as follows.

       ORIENT overlay  gives the title of the input overlay.

       PRECISELY       specifies that output values from 0 to 360 are to be
		       generated. If omitted, values from 0 to 8 are generated.

       FOR overlay     gives the title of the output overlay. If omitted, the
		       title X is assumed.
