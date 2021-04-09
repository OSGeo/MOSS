OPERATION PROFILE

The PROFILE operation creates an output overlay by characterizing the
cross-sectional profiles of points on a three-dimensional surface. The profile
of a point is the angle formed by an imaginary line extending:
       * from the center of one of the point's eight adjacent neighbors;
       * to the center of of the point itself; and;
       * on to the center of the diametrically opposing neighbor.
The third dimensional position of each point is defined by its value on an
input overlay.

Profiles are specified by orientation and can be characterized by type or by
size. The orientation of a profile determines which of each point's neighbors
are involved and establishes a profile direction. Profile size is then measured
in degrees as the skyward angle between the profile's incoming and outgoing
line segments.	Profile types are characterized according to the upward,
downward, or horizontal inclination of these line segments.  Here, values are
assigned such that:

       1 represents profiles that extend "up and up";
       2 represents profiles that extend "up and over";
       3 represents profiles that extend "up and down";
       4 represents profiles that extend "over and up";
       5 represents profiles that extend "over and over";
       6 represents profiles that extend "over and down";
       7 represents profiles that extend "down and up";
       8 represents profiles that extend "down and over"; and
       9 represents profiles that extend "down and down."

The general form of the instruction for this operation is:

       PRO[FILE] overlay I[NFLECTION] [direction] [A[T] rise] [F[OR] overlay]

where individual phrases are interpreted as follows.

       PROFILE overlay
		       gives the title of the input overlay. The values on this
		       overlay should relate to units of distance equivalent to
		       those of its scale.

       INFLECTION      indicates that profiles are to be characterized
		       according to size. If omitted, profiles are
		       characterized according to type.

       direction       is one of the terms NORTH, NORTHEAST, EAST, SOUTHEAST,
		       SOUTH, SOUTHWEST, WEST, or NORTHWEST indicating profile
		       orientation as follows:

		       NORTH profiles extend from each point's left
		       neighbor toward its right neighbor;

		       NORTHEAST profiles extend from each point's upper
		       left neighbor toward its lower right neighbor;

		       EAST profiles extend from each point's upper
		       neighbor toward its lower neighbor;

		       SOUTHEAST profiles extend from each point's upper
		       right neighbor toward its lower left neighbor;

		       SOUTH profiles extend from each point's right
		       neighbor toward its left neighbor;

		       SOUTHWEST profiles extend from each point's lower
		       right neighbor toward its lower left neighbor;

		       WEST profiles extend from each point's lower
		       neighbor toward its upper neighbor; and

		       NORTHWEST profiles extend from each point's lower
		       left neighbor toward its upper right neighbor.

       AT rise	       gives the maximum change in third-dimensional value
		       which, over a distance of one grid space, can still be
		       regarded as horizontal for purposes of characterizing
		       profile type.

       FOR overlay     gives the title of the output overlay. If omitted, the
		       title X is assumed.
