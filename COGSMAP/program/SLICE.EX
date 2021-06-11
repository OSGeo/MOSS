OPERATION SLICE

The SLICE operation creates an output overlay by dividing a specified range of
values on an input overlay into equal intervals, then replacing each input
value with a new value indicating the ordinal position of its interval.

The general form of the instruction for this operation is:

       SL[ICE] overlay	 [I[NTO] intervals]   [FR[OM] value]
       [TH[ROUGH] value]   [F[OR] overlay]

where all phrases are to appear on a single line of text and where individual
phrases are interpreted as follows.

       SLICE overlay   gives the title of the input overlay.

       INTO intervals
		       gives a number indicating maximum output value to be
		       generated. If omitted or specified as less than 1 or
		       greater than 1998, a value of 4 is assumed.

       FROM value      gives a number indicating the lower limit of the range
		       of input values to be sliced into intervals. If omitted,
		       the lowest input value encountered is assumed.

       TO value        gives a number indicating the upper limit of the range
		       of input values to be sliced into intervals. If omitted,
		       the highest input value encountered is assumed.

       FOR overlay     gives the title of the output overlay. If omitted, the
		       title X is assumed.
