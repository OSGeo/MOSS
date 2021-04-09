OPERATION RECODE

The RECODE operation creates an output overlay by assigning new, specified
values to the regions of an input overlay.  Regions for which no new  value is
specified retain their input values.

The general form of the instruction for this operation is:

       RECO[DE] overlay   [F[OR] overlay]   [/]
       [A[SSIGNING] value   [T[O] value]...   [TH[ROUGH] value]]...

where individual phrases are interpreted as follows.

       RECODE overlay
		       gives the title of the input overlay;

       FOR overlay     gives the title of the output overlay. If omitted, the
		       title X is assumed.

       ASSIGNING value
		       gives an output value to be assigned.

       TO value        gives the input value of the region to which the most
		       recently specified output value is to be assigned.

       THROUGH value   gives an input value which effectively causes the most
		       recently specified TO value phrase to be repeated for
		       all values greater than that value and less than or
		       equal to this one.

       /	       indicates that the rest of the instruction is to be read
		       by skipping immediately to the next line of input text.
