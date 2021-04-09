OPERATION EXPONENTIATE

The EXPONENTIATE operation creates an output overlay by raising the values of
one input overlay to exponential powers represented by the values of another
input overlay on a point-by-point basis.  If more than two input overlays are
specified, each additional overlay is used to further exponentiate whatever
output overlay values have been computed so far.  All input overlays specified
are processed in this manner according to the order in which they appear in the
EXPONENTIATE instruction.

On output, all fractional values are truncated.  Negative exponents are
permitted but may simply result in an output overlay value of 0, depending on
the computing system used.  Attempts to generate output overlay values above
32767 or below -32768 may also produce unexpected results, depending on the
computing system used.

The general form of the instruction for this operation is:

       EXPON[ENTIATE] overlay	B[Y] overlay   [B[Y] overlay]...   [/]
       [F[OR] overlay]

where individual phrases are interpreted as follows.

       EXPONENTIATE overlay
		       gives the title of the initial input overlay whose
		       values are to be exponentiated.

       BY overlay      gives the title of an additional input overlay of values
		       to be processed as exponents.

       FOR overlay     gives the title of the output overlay. If omitted, the
		       title X is assumed.

       /	       indicates that the rest of the instruction is to be read
		       by skipping immediately to the next line of input text.
