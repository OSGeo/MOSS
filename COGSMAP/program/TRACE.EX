OPERATION TRACE

The TRACE operation creates an output overlay by assigning new values to points
located along specified line segments. These line segments are defined by
endpoint coordinates, which can be located in reference to a specified
Cartesian coordinate system.

The general form of the instruction for this operation is:

       T[RACE]	 [A[T] number]	 [T[ARGETTING] x y]   [F[OR] overlay]
       format
       [x y value]
       .
       .
       .

where individual phrases are interpreted as follows.

       AT number       gives the number of distance units per grid space in the
		       coordinate system to be used.  If omitted, 10 units per
		       grid space is assumed.

       TARGETTING x y  gives the x (row) and y (column) coordinates of the
		       center of the point at row 1, column 1.	If omitted or
		       if either coordinate is given as less than 0, both
		       coordinates are assumed to be 5.

       FOR overlay     gives the title of the output overlay. If omitted, the
		       title X is assumed.

       format	       is a FORTRAN (programming language) input format
		       specification indicating how every subsequent x-y-value
		       phrase is to be interpreted. A typical format
		       specification might be given as

					 (BZ,2I3,2X,I1)

		       where:

		       ( and ) enclose the format specification;

		       BZ      means that blank spaces are to be read as 0's;

			,	means "and then";

		       2I3     means "read two consecutive 3-digit integers";

		       2X      means "skip two spaces."

		       I1      means "read a 1-digit integer."

		       For more complete information on FORTRAN input format
		       specifications, consult the IBM Personal Computer
		       Professional FORTRAN manual.

       x y value       gives an x coordinate, then a y coordinate and the value
		       to be assigned to the line of points terminating at that
		       x,y location. Each x-y-value phrase must begin on a new
		       input line, and each must conform to the format
		       specification given in order to be interpreted
		       correctly.  If that format specification does not
		       account for an x coordinate, a y coordinate, and a
		       value, it will automatically be repeated as necessary.

		       If an x coordinate of 0 is specified, no value is
		       assigned.

		       x-y-value phrases will continue to be read repeatedly
		       until one is encountered in which the x coordinate given
		       is less than 0.
