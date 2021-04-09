OPERATION SCAN

The SCAN operation creates an output overlay by computing a value for each of a
selected set of points as a specified function of input overlay values in that
point's neighborhood. The size and shape of this neighborhood can also be
specified.

Output values may indicate the total, average, maximum, minimum, majority,
diversity, deviation, or relative amount of area associated with the input
values in each neighborhood. Where necessary, these values are rounded to the
nearest integer.

The general form of the instruction for this operation is:

       SCAN overlay   [statistic]   [A[ROUND] overlay]	 [/]
       [W[ITHIN] diameter]   [D[IAGONALLY]]   [B[EYOND] diameter]   [/]
       [F[OR] overlay]

where individual phrases are interpreted as follows.

       SCAN overlay    gives the title of the input overlay whose values are to
		       be summarized within each neighborhood. Values less than
		       0 or greater than 2000 are treated as missing values.
		       The area beyond the edges of this overlay is also
		       treated as a region of missing values.

       statistic       is one of the terms TOTAL, AVERAGE, MAXIMUM, MINIMUM,
		       MEDIAN, MAJORITY, MINORITY, DIVERSITY, DEVIATION, and
		       PROPORTION indicating the function to be used in
		       calculating output values as follows.

		       TOTAL indicates that each neighborhood should be
		       characterized according to the point-by-point sum of its
		       input values. This sum is automatically increased in
		       proportion to the number of neighborhood values that are
		       missing.

		       AVERAGE indicates that each neighborhood should be
		       characterized according to the point-by-point mean of
		       its input values.

		       MAXIMUM indicates that each neighborhood should be
		       characterized according to the highest of its input
		       values.

		       MINIMUM indicates that each neighborhood should be
		       characterized according to the lowest of its input
		       values.

		       MEDIAN indicates that each neighborhood should be
		       characterized according to the point-by-point median of
		       its input values.

		       MAJORITY indicates that each neighborhood should be
		       characterized according to whichever of its input values
		       occurs most frequently, with ties resolved in favor of
		       higher value.

		       MINORITY indicates that each neighborhood should be
		       characterized according to whichever of its input values
		       occurs least frequently, with ties resolved in favor of
		       higher value.

		       DIVERSITY indicates that each neighborhood should be
		       characterized according to the number of different input
		       values it contains.

		       DEVIATION indicates that each neighborhood should be
		       characterized according to the difference between the
		       input value of its central point and the point-by-point
		       mean of all of its input values.

		       PROPORTION indicates that each neighborhood should be
		       characterized according to the areal percent of the
		       neighborhood that shares the input value of its central
		       point.

		       If more than one of these terms is specified, only the
		       final one will take effect. If omitted, AVERAGE values
		       are assumed.

       AROUND overlay
		       gives the title of an input overlay on which the points
		       whose neighborhoods are to be scanned are identified by
		       values greater than 0. All other points are set to a
		       value of 0 on the output overlay. If omitted, the
		       neighborhoods of all points are scanned.

       WITHIN diameter
		       gives a number indicating the diameter, in grid spaces,
		       of the neighborhood surrounding each point. If this
		       number is odd, the neighborhood center will be at the
		       center of this point; if even, it will be at the point's
		       lower right corner. In either case, each point's
		       neighborhood includes only those points whose centers
		       lie within the specified diameter. If omitted, a
		       diameter of 3 is assumed.

       DIAGONALLY      indicates that the otherwise-circular shape of each
		       neighborhood should be extended to form a square
		       encompassing all points whose centers lie within upper
		       or lower and left or right projections of the
		       neighborhood diameter.

       BEYOND diameter
		       gives a number indicating the diameter, in grid spaces,
		       of a hole within the neighborhood surrounding each
		       point. If this number is odd, the hole's center will be
		       at the center of this point; if even, it will be at the
		       point's lower right corner. In either case, each point's
		       neighborhood includes only those points whose centers
		       lie beyond the specified diameter. If omitted, a
		       diameter of 0 is assumed.

       FOR overlay     gives the title of the output overlay. If omitted, the
		       title X is assumed.

       /	       indicates that the rest of the instruction is to be read
		       by skipping immediately to the next line of input text.
