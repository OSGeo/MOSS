OPERATION STRIP

The STRIP operation creates an output overlay by assigning new values to the
groups of consecutive points on specified rows of an input overlay.

The general form of the instruction for this operation is:

       STRI[P] overlay	 [O[N] overlay]   [F[OR] overlay]
       [row [value column]...]
       .
       .
       .

where individual phrases are interpreted as follows.

       ON overlay      gives the title of the input overlay. If omitted, an
		       overlay of all 0's is assumed.

       STRIP overlay   gives the title of the input overlay.

       FOR overlay     gives the title of the output overlay.
			If omitted, the title X is assumed.

       row	       gives a three-digit row number which must be specified
		       at the beginning of a new line of text. If specified as
		       000, the previous row specification will be assumed.
		       These phrases will continue to be read repeatedly until
		       the row number given is less than 0.

       column value    gives a pair of three-digit numbers without preceding or
		       intervening spaces, up to 12 of which may be specified
		       on a single line of input text. The first of the numbers
		       identifies a column and the second an output value. The
		       value is assigned to all points on the most recently
		       specified row that are on or to the left of the
		       specified column and to the right of any previously
		       specified column on that row.

		       A specified value of 998 indicates that each point
		       affected is to take on the value of the point just above
		       it. A specified value of 999 indicates that each point
		       is to retain its current value.
