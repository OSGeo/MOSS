OPERATION AVERAGE

The AVERAGE operation creates an output overlay by averaging the overlay
weighted values of two or more input overlays on a point-by-point basis.
Output values are computed by multiplying the values of each input overlay by a
numerical weight assigned to that overlay, adding the resultant products for
each individual point, then dividing each point`s total by the sum of all
assigned weights

The general form of the instruction for this operation is:

       AV[ERAGE] overlay   [T[IMES] weight]   P[LUS] overlay   [/]
       [T[IMES] weight]   [P[LUS] overlay   [T[IMES] weight]]...   [/]
       [F[OR] overlay]

where individual phrases are interpreted as follows.

       AVERAGE overlay
		       gives the title of one of the input overlays to be
		       averaged.

       TIMES weight    gives a number indicating the weighting factor to be
		       associated with the most recently specified input
		       overlay.  If omitted, a weight of 1 is assumed.

       PLUS overlay    gives the title of an additional input overlay to be
		       averaged.

       FOR overlay     gives the title of the output overlay. If omitted, the
		       title X is assumed.

       /	       indicates that the rest of the instruction is to be read
		       by skipping immediately to the next line of input text.
