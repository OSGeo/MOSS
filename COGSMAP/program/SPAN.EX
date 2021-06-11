OPERATION SPAN

The SPAN operation creates an output overlay by measuring the narrowness of
input overlay regions at selected points. The narrowness of a region at a point
is the length of the shortest line segment that can be constructed through the
center of that point to two diametrically opposing edges of the region
(multiplied by two for each edge that coincides with the edge of an overlay).

On output, a value is assigned to each selected point indicating its narrowness
in grid spaces. Other points are assigned a value of 0.

The general form of the instruction for this operation is:

       SPA[N] overlay  [A[ROUND] overlay]  [T[O] width]  [F[OR] overlay]

where individual phrases are interpreted as follows.

       SPAN overlay    gives the title of the input overlay defining the
		       regions to be spanned.

       AROUND overlay
		       gives the title of an input overlay on which points for
		       which narrowness is to be measured are identified by
		       values greater than 0. If omitted, narrowness is
		       measured for all points.

       TO distance     gives a number indicating the maximum narrowness value
		       to be measured. All greater measurements are set to this
		       maximum. If omitted or specified as less than 1 or
		       greater than 19, a distance of 4 is assumed.

       FOR overlay     gives the title of the output overlay. If omitted, the
		       title X is assumed.
