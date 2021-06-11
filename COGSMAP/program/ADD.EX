OPERATION ADD

The ADD operation creates an output overlay by adding the values of two or more
input overlays on a point-by-point basis.

The general form of the instruction for this operation is:

       AD[D] overlay   T[O] overlay   [T[O] overlay]...   [/]
       [F[OR] overlay]

where individual phrases are interpreted as follows.

       ADD overlay     gives the title of one of the input overlays to be
		       added.

       TO overlay      gives the title of an additional input overlay to be
		       added.

       FOR overlay     gives the title of the output overlay. If omitted, the
		       title X is assumed.

       /	       indicates that the rest of the instruction is to  be
		       read by skipping immediately to the next line of input
		       text.
