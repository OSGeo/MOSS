OPERATION EULER

The EULER operation creates an output overlay by computing the Euler number of
each region on an input overlay. The Euler number (or topological genus) of an
area is computed as the number of wholly enclosed "islands" (i.e. contiguous
areas of foreign value) which lie within that area less one short of the number
of islands which make up the area itself.  An area shaped like a pair of
figure-eights, for example, would have an Euler number of 3 due to its 4
"holes" and 1 "extra island."
The general form of the instruction for this operation is:

       EU[LER] overlay	 [F[OR] overlay]

where individual phrases are interpreted as follows.

       EULER overlay   gives the title of the input overlay.

       FOR overlay     gives the title of the output overlay. If omitted, the
		       title X is assumed.
