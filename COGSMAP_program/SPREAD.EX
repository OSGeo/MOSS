OPERATION SPREAD

The SPREAD operation creates an output overlay by measuring the shortest
distance to each point from any of a selected set of points. Distances are
measured between point centers and can be calculated in either of two ways. The
first involves units such as feet or meters, expressed in terms of grid spaces
rounded to the nearest integer. The second involves units such as minutes,
dollars, gallons of fuel, or levels of environmental impact.

In the second of these cases, the number of "distance" units per grid space may
vary from one point to another. Distance is therefore calculated by starting at
the set of selected points and then repeatedly moving to adjacent neighbors,
accumulating distance increments. This is done such that each point's
accumulated distance is ultimately computed as the minimum sum of one of its
neighbor's accumulated distances and its own incremental distance multiplied by
1.4142 if the neighbor involved is diagonally adjacent. This distance
accumulating process can also be limited to upward or downward directions over
a three-dimensional surface.

On output, the set of points selected are set to a value of 0. These are
surrounded by concentric zones of increasingly higher values up to a specified
maximum, any all points beyond are set to that maximum.

The general form of instruction for this operation is:

       SPR[EAD] overlay   [T[O] distance]   [O[VER] overlay]   [D[OWNHILL]
       [TH[ROUGH] overlay]   [F[OR] overlay]

where all phrases are to appear on a single line of text and where individual
phrases are interpreted as follows.

       SPREAD overlay  gives the title of an input overlay on which the points
		       from which distances are to be measured are identified
		       by values greater than 0.

       TO distance     gives a number indicating the maximum distance value to
		       be measured. If omitted or set to 0 or greater than 327,
		       a maximum of 4 is assumed.

       OVER overlay    gives the title of an input overlay whose values define
		       the position of each point on a three-dimensional
		       surface used to direct the distance measuring process.
		       If omitted, a horizontal surface is assumed.

       DOWNHILL        indicates that distances are to be measured only in
		       directions that face downward or across slope over the
		       surface defined by the OVER overlay. If omitted,
		       distances are measured only in directions that face
		       upward or across slope over that surface.

       THROUGH overlay
		       gives the title of an input overlay whose values
		       represent the number of incremental distance units per
		       grid space at each point. If omitted, 1 unit is assumed.

       FOR overlay     gives the title of the output overlay. If omitted, the
		       title X is assumed.
