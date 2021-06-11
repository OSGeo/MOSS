OPERATION GRID

The GRID operation creates an output overlay by assigning new values to the
individual points on specified rows of an input overlay.

The general form of the instruction for this operation is:

       G[RID]	[O[N] overlay]	 [F[OR] overlay]
       format
       row values
       .
       .
       .

where individual phrases are interpreted as follows.

       ON overlay      gives the title of the input overlay. If omitted, an
		       overlay of all 0's is assumed.

       FOR overlay     gives the title of the output overlay. If omitted, the
		       title X is assumed.

       format	       is a FORTRAN (programming language) input format
		       specification indicating how every subsequent row-values
		       phrase is to be interpreted. A typical format
		       specification might be given as

				    (BZ,I5,25I3/5X,25I3)
			where:

		       ( and ) enclose the format specification;

		       BZ      means that blank spaces are to be read as
			       zeroes;

		       I5      means "read a 5-digit integer";

		       ,       means "and then";

		       25I3    means "read twenty-five consecutive
			       3-digit integers";

		       /       means "then skip to the beginning of the
			       next line of input text and"; and,

		       5X      means "skip five spaces."

		       For more complete information on FORTRAN input
		       format specifications, consult the IBM Personal
		       Computer Professional FORTRAN manual.

       row values      gives a row number and the values to be assigned to all
		       points of that row in consecutive, left- to-right
		       order.  Each row-values phrase must begin on a new input
		       line, and each must conform to the format specification
		       given in order to be interpreted correctly.  If that
		       format specification does not account for a row number
		       and a complete row of values, it will automatically be
		       repeated as necessary.

		       row-values phrases will continue to be read repeatedly
		       until one is encountered in which the row number given
		       is less than 0.
