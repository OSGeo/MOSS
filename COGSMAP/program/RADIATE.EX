OPERATION RADIATE

The RADIATE operation creates an output overlay by delineating the viewshed of
selected points on a three-dimensional surface. A viewshed is the set of all
points on which objects of specified heights are visible from one or more of
the viewpoints selected. Visibility is determined according to whether or not
an unobstructed line of sight can be traced from a specified height above the
center of a viewpoint to the height of an object above the center of that
point. Lines of sight may be obstructed by the three-dimensional surface itself
and/or barriers of specified width and heights above it. The designation of
viewpoints, the heights of both objects and barriers, and the position of each
point on the three-dimensional surface are defined by input overlays.

On output, points which are visible from at least one viewpoint are set to a
value of 1. All others are set to 0.

The general form of the instruction for this operation is:

       RA[DIATE] overlay   [A[T] height]   [T[O] distance]   [/]
       [O[VER] overlay]   [ON[TO] overlay]   [TH[ROUGH] overlay]   [/]
       [S[CREENING] diameter]	[AB[OVE] angle]   [F[OR] overlay]

where individual phrases are interpreted as follows.

       RADIATE overlay
		       gives the title of an input overlay on which values
		       greater than 0 designate viewpoints. The scale of this
		       overlay should relate to the same units of distance used
		       for third-dimensional measurements in AT, OVER, ONTO,
		       THROUGH, and SCREENING specifications.

       AT height       gives a number indicating the above-surface height from
		       which each viewpoint's lines of sight are to be
		       calculated. This number should relate to distance units
		       equivalent to those of the viewpoint overlay's scale. If
		       omitted of specified as less than 0, a height of 0 is
		       assumed.

       TO distance     gives a number indicating the maximum line of sight
		       distance in grid spaces measured horizontally. All
		       points beyond this distance from a viewpoint are
		       regarded as not visible. If omitted or specified as less
		       than 0, a distance of 10 is assumed.

       OVER overlay    gives the title of the input overlay whose values define
		       the third-dimensional position of each point on a
		       surface. These values should relate to distance units
		       equivalent to those of the viewpoint overlay's scale.
		       Negative values are treated as 0's. If omitted, a
		       horizontal surface is assumed.

       ONTO overlay    gives the title of the input overlay whose values define
		       the above-surface heights of potentially visible
		       objects. These values should relate to distance units
		       equivalent to those of the viewpoint overlay's scale.
		       Negative values are treated as 0's. If omitted, heights
		       of 0 are assumed. Potentially visible objects are not
		       regarded as visual barriers.

       THROUGH overlay
		       gives the title of the input overlay whose values define
		       the above-surface heights of visual barriers. These
		       values should relate to distance units equivalent to
		       those of the viewpoint overlay's scale. Negative values
		       are treated as 0's. If omitted, heights of 0 are
		       assumed.

       SCREENING diameter
		       gives a number indicating the width of all visual
		       barriers measured perpendicularly to each line of sight.
		       This number should relate to distance units equivalent
		       to those of the viewpoint overlay's scale.If omitted or
		       specified as less than 0, a width equivalent to the
		       diagonal dimension of a point is assumed.

       ABOVE angle     gives a number indicating, in degrees, the minimum angle
		       that must exist between a line of sight to the top of an
		       object and the line of sight to the top of every
		       intervening barrier before that object is regarded as
		       seen. If omitted, an angle of 0 is assumed.

       FOR overlay     gives the title of the output overlay. Because of some
		       internal file handling idiosyncrasies, this title should
		       not be the same as that used in any THROUGH
		       specification. If omitted, the title X is assumed.

       /	       indicates that the rest of the instruction is to be read
		       by skipping immediately to the next line of input text.
