OPERATION DRAIN

The DRAIN operation creates an output overlay by computing runoff volume at
points located on a three-dimensional surface. Runoff volume can be visualized
as the amount of liquid that would ultimately flow into each point if known
amounts of this liquid were to be released at specified surface locations. The
third dimensional position of each point on this surface is defined by its
value on one of two input overlays, while the volume initially drained from
each point is defined by its value on the other.

The general form of the instruction for this operation is:

       DR[AIN] overlay	 O[VER] overlay   [F[OR] overlay]

where individual phrases are interpreted as follows.

       DRAIN overlay   gives the title of the input overlay whose values define
		       initial volumes.

       OVER overlay    gives the title of the input overlay whose values define
		       the third-dimensional position of each point on the
		       surface over volumes are to be drained.

       FOR overlay     gives the title of the output overlay. If omitted, the
		       title X is assumed.
