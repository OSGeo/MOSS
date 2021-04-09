OPERATION DISPLAY

The DISPLAY operation produces a line-printer image of an input overlay on
which each point is represented according to its value by a symbol of up to
four overprinted characters. It is configured for use with the IBM-PC graphics
printer such that the size of each symbol is one tenth of an inch in both
horizontal and vertical dimensions. If the image displayed is wider than 60
columns, it will presented on two or more panels.

The general form of the instruction for this operation is:

       DIS[PLAY] overlay   [A[BOVE] row]   [B[ELOW] row]   [/]
       [F[ROM] column]	 [T[HROUGH] column]   [O[VERPRINTING]	[/]
       [S[PECIALLY]]
       [value space characters]
       .
       .
       .

where individual phrases are interpreted as follows.

       DISPLAY overlay
		       gives the title of the input overlay to be displayed.

       BELOW row       gives the number of the top row of the area to be
		       displayed.  If omitted, row 1 is assumed.

       ABOVE row       gives the number of the bottom row of the area to be
		       displayed.  If omitted, the bottom most row of the
		       overlay is assumed.

       FROM column     gives the number of the left most column of the area to
		       be displayed. If omitted, column 1 is assumed.

       TO column       gives the number of the right most column of the area to
		       be displayed. If omitted, the right most column of the
		       overlay is assumed.

       OVERPRINTING    indicates that each display symbol is to be composed of
		       four overprinted characters. If omitted, one-character
		       symbols are used.

       SPECIALLY       indicates that display symbols are to be composed of
		       user-defined characters specified in the
		       value-space-characters format described below.  If
		       omitted, standard symbols are used.

       value space characters
		       gives a two-digit value (with no preceding characters or
		       spaces) followed immediately by a blank space and the
		       character (or up to four characters if overprinting) to
		       be used to represent the region associated with that
		       value.

		       When SPECIALLY has been specified, the DISPLAY operation
		       will continue to expect value-space-characters input new
		       lines of text until the -1 phrase described below is
		       encountered.

		       If no special character(s) are specified for a
		       particular region, it will be represented by its
		       standard symbol.

		       value-space-characters phrases will continue to be read
		       repeatedly until one is encountered in which the value
		       given is less than 0.
