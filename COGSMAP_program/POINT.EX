OPERATION POINT

The POINT operation creates an output overlay by assigning user-specified
values to the individual points of an input overlay.

The general form of the instruction for this operation is:

       PO[INT]	 [O[N] overlay]   [F[OR] overlay]
       format
       row column value
       .
       .
       .

where individual phrases are interpreted as follows.

       ON overlay      gives the title of the input overlay.

       FOR overlay     gives the title of the output overlay. If omitted, the
		       title X is assumed.

       format	       is a FORTRAN (programming language) input format
		       specification indicating how every subsequent
		       row-column-value phrase is to be interpreted. A typical
		       format specification might be given as

					 (BZ,2I3,2X,I1)

		       where:

		       ( and ) enclose the format specification;

		       BZ      means that blank spaces are to be read as 0's;

		       ,       means "and then";

		       2I3     means "read two consecutive 3-digit integers";

		       2X      means "skip two spaces."

		       I1      means "read a 1-digit integer."

		       For more complete information on FORTRAN input format
		       specifications, consult the IBM Personal Computer
		       Professional FORTRAN manual.

       row column value
		       gives a row number, then a column number and the value
		       to be assigned to the point at that row-column
		       position.  Each row-column-value phrase must begin on a
		       new input line, and each must conform to the format
		       specification given in order to be interpreted
		       correctly.  If that format specification does not
		       account for a row number, a column number and a value,
		       it will automatically be repeated as necessary.

		       row-column-value phrases will continue to be read
		       repeatedly until one is encountered in which the row
		       number given is less than 0.
