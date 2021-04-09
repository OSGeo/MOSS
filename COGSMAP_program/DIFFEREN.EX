OPERATION DIFFERENTIATE

The DIFFERENTIATE operation creates an output overlay by computing slopes at
points located on a three-dimensional surface. Slopes are ratios of vertical
(i.e. perpendicular to the cartographic plane) to horizontal distance. The
third dimensional position of each point on this surface is defined by its
value on an input overlay.

Output values are assigned as percentages rounded to the nearest integer. They
may be computed to reflect either the steepest of the slopes defined by a point
and its immediate neighbors or the vector sum of those slopes.

The general form of the instruction for this operation is:

       DIF[FERENTIATE] overlay	 M[AXIMALLY]   [F[OR] overlay]

where individual phrases are interpreted as follows.

       DIFFERENTIATE overlay
		       gives the title of the input overlay. The values on this
		       overlay should relate to units of distance equivalent to
		       those of its scale. Input values of less than 0 exclude
		       points from slope calculations.

       MAXIMALLY       indicates that steepest slopes are to be computed.  If
		       omitted, vector sums are used.

       FOR overlay     gives the title of the output overlay. If omitted, the
		       title X is assumed.
