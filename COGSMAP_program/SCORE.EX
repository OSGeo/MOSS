OPERATION SCORE

The SCORE operation compares the values of one input overlay with those of
another input overlay on a point-by-point basis.  For each region of the first
input overlay, it then summarizes the values of the second input overlay which
occur over the same set of points.

On output, this summary may be presented in cross-tabular form and/or used to
create a new overlay.  Output overlay values may be assigned to each region of
the first input overlay to indicate the total, average, maximum, minimum,
majority, minority, or diversity of its second-overlay values;	they may also
be assigned to areas defined by particular combinations input values according
to the degree of deviation, proportion, or overlap represented by those
combinations.

The general form of the instruction for this operation is:

       SCO[RE] overlay B[Y] overlay [statistic] [Q[UIETLY] [F[OR] overlay]

where individual phrases are interpreted as follows.

       SCORE overlay   gives the title of the first input overlay. Regions of
		       values of less than 0 or greater than 20 on this overlay
		       are ignored.

       BY overlay      gives the title of the second input overlay. Values of
		       less than 0 or greater than 20 on this overlay are
		       regarded as missing.

       statistic       is one of the terms TOTAL, AVERAGE, MAXIMUM, MINIMUM,
		       MEDIAN, MAJORITY, MINORITY, DIVERSITY, DEVIATION, and
		       PROPORTION indicating the function to be used in
		       calculating output values as follows.

		       TOTAL indicates that each region should be characterized
		       according to the point-by-point sum of its input values.
		       This sum is automatically increased in proportion to the
		       number of each region's values that are missing.

		       AVERAGE indicates that each region should be
		       characterized according to the point-by-point mean of
		       its input values.

		       MAXIMUM indicates that each region should be
		       characterized according to the highest of its input
		       values.

		       MINIMUM indicates that each region should be
		       characterized according to the lowest of its input
		       values.

		       MEDIAN indicates that each region should be
		       characterized according to the point-by-point median of
		       its input values.

		       MAJORITY indicates that each region should be
		       characterized according to whichever of its input values
		       occurs most frequently, with ties resolved in favor of
		       higher value.

		       MINORITY indicates that each region should be
		       characterized according to whichever of its input values
		       occurs least frequently, with ties resolved in favor of
		       higher value.

		       DIVERSITY indicates that each region should be
		       characterized according to the number of different input
		       values it contains.

		       DEVIATION indicates that each point should be
		       characterized according to the difference between its
		       own input value and the point-by-point mean of all input
		       values in its region.

		       PROPORTION indicates that each point should be
		       characterized according to the areal percent of its
		       region that shares its input value.

		       OVERLAP indicates that each point should be
		       characterized according to the degree to which its input
		       overlay regions coincide. This is computed as an average
		       of the areal percent of its first input overlay region
		       that shares its second input overlay value and the areal
		       percent of its second input overlay region that shares
		       its first input overlay value.

		       If more than one of these terms is specified, only the
		       final one will take effect. If omitted, no output
		       overlay will be generated.

       QUIETLY	       indicates that no cross-tabular listing is to be
		       generated. If omitted, a listing is generated.

       FOR overlay     gives the title of the output overlay. If omitted, the
		       title X is assumed.
