OPERATION RESPACE

The RESPACE operation creates an output overlay by uniformly shifting the
values of points on an input overlay to new row and/or column positions. Origin
and destination positions are specified by upper, lower, left, and right bounds
as well as nth row and column spacing. Respaced values may also be superimposed
onto a second input overlay.

The general form of the instruction for this operation is:

       RES[PACE] overlay   O[NTO] overlay   [F[OR] overlay]   [/]
       FROMS[PACING] nth   FROMT[OP] row   FROMB[OTTOM] row   [/]
       FROML[EFT] column   FROMR[IGHT] column	[/]
       TOS[PACING] nth	 TOT[OP] row   TOB[OTTOM] row	[/]
       TOL[EFT] column	 TOR[IGHT] column

where individual phrases are interpreted as follows.

       RESPACE overlay
		       gives the title of the input overlay whose values are to
		       be respaced.

       ONTO overlay    gives the title of the input overlay onto which respace
		       values are to be superimposed. If omitted, an overlay of
		       all 0's is assumed..

       FOR overlay     gives the title of the output overlay. If omitted, the
		       title X is assumed.

       FROMSPACING nth
		       gives a number indicating the distance, in grid spaces,
		       between consecutive row and column positions in the
		       pattern of input values to be respaced. If omitted or
		       specified as less than 1, a spacing of 1 is assumed.

       FROMTOP row     gives the number of the uppermost row of input values to
		       be respaced. If omitted or specified as less than 1 or
		       greater than the number of rows per map, half the sum of
		       1 plus the FROMSPACING value is assumed.

       FROMBOTTOM row
		       gives the number of the lowermost row of input values to
		       be respaced. If omitted or specified as less than 1 or
		       greater than the number of rows per map, the lowermost
		       row of the map is assumed.
       FROMLEFT column
		       gives the number of the leftmost column of input values
		       to be respaced. If omitted or specified as less than 1
		       or greater than the number of columns per map, half the
		       sum of 1 plus the FROMSPACING value is assumed.

       FROMRIGHT column
		       gives the number of the rightmost column of input values
		       to be respaced. If omitted or specified as less than 1
		       or greater than the number of columns per map, the
		       rightmost row of the map is assumed.

       TOSPACING nth
		       gives a number indicating the distance, in grid spaces,
		       between consecutive row and column positions in the
		       pattern of output values to be generated. If omitted of
		       specified as less than 1, a spacing of 1 is assumed.

       TOTOP row       gives the number of the uppermost row of the area into
		       which values are to be respaced. If omitted or specified
		       as less than 1 or greater than the number of rows per
		       map, half the sum of 1 plus the TOSPACING value is
		       assumed.

       TOBOTTOM row    gives the number of the lowermost row of area into which
		       values are to be respaced. Values to be positioned below
		       this row are ignored. If omitted or specified as less
		       than 1 or greater than the number of rows per map, the
		       lowermost row of the map is assumed.

       TOLEFT column
		       gives the number of the leftmost column of area into
		       which values are to be respaced. If omitted or specified
		       as less than 1 or greater than the number of columns per
		       map, half the sum of 1 plus the TOSPACING value is
		       assumed.

       TORIGHT column
		       gives the number of the rightmost column of area into
		       which values are to be respaced. Values to be positioned
		       to the right of this row are ignored. If omitted or
		       specified as less than 1 or greater than the number of
		       columns per map, the rightmost row of the map is
		       assumed.

       /	       indicates that the rest of the instruction is to be read
		       by skipping immediately to the next line of input text.
