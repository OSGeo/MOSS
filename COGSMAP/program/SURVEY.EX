OPERATION SURVEY

The SURVEY operation creates an output overlay by calculating the convexity of
each region on an input overlay. The convexity of a region is the ratio of the
square root of the region's area to the length of its perimeter. Convexity is
greatest for circular forms and decreases as shapes become more complex.

On output, values ranging from a maximum of 100 to a minimum approaching 0 are
assigned to each point according to the convexity of its region.

The general form of the instruction for this operation is:

       SUR[VEY] overlay   [F[OR] overlay]

where individual phrases are interpreted as follows.

       SURVEY overlay  gives the title of the input overlay.

       FOR overlay     gives the title of the output overlay. If omitted, the
		       title X is assumed.
