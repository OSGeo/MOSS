OPERATION CLUMP

The CLUMP operation creates an output overlay by uniquely identifying the
individual "clumps" of points on an input overlay.  A clump is a group of one
or more points that share a common value on the input overlay and which cover
an area that is unbroken (or nearly so) by points of any other value.

Output values are determined according to the sequence in which clumps are
first encountered when the input overlay is scanned row by row, from top to
bottom, and from left to right across each row.  Each clump is numbered
according to its ordinal position within this sequence such that output overlay
values range continuously from 1 to the total number of clumps identified.

The general form of the instruction for this operation is:

       CL[UMP] overlay	[A[T] distance]  [D[IAGONALLY]]  [F[OR] overlay]

where individual phrases are interpreted as follows.

       CLUMP overlay   gives the title of the input overlay to be clumped.
		       Points with input values of less than 0 or greater than
		       500 are set to 0 on output.

       AT distance     gives a maximum distance (measured in grid spaces
		       between point centers) within which otherwise separate
		       clumps are to be regarded as part of a single clump. If
		       omitted or specified as less than 1 or greater than 10,
		       a distance of 1 is assumed.

       DIAGONALLY      indicates that the otherwise circular neighborhood
		       defined by the "AT distance" phrase is to be extended
		       diagonally to encompass a square.

       FOR overlay     gives the title of the output overlay. If omitted, the
		       title X is assumed.
