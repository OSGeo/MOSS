OPERATION SHOW

The SHOW operation produces a graphic image of an input overlay on which each
point is represented by a color according to its value. This image may be shown
in plan view or in perspective. It can also be projected onto a
three-dimensional surface defined by a second input overlay.

SHOW output can be directed to any of several graphic devices by way of the IBM
Virtual Device Interface (VDI) system but should not be used unless that system
is installed.

The general form of the instruction for this operation is:

       SH[OW] overlay	[O[N] overlay]
       [[letter] [number]]
       .
       .
       .

where individual phrases are interpreted as follows.

       SHOW overlay    gives the title of overlay whose values define the
		       colors to be shown.

       ON overlay      gives the title of the overlay whose whose
			values define the third-dimensional position
			of each point on the surface to be drawn.  If
			omitted, a flat surface is assumed.

       letter	       is one of the letters O, I, T, B, L, R, V, F, E, S, C,
		       M, D, G, P, or Q specifying a drawing control parameter
		       as follows.

		       O (for orientation) indicates that the image is to be
		       drawn rotated counter-clockwise by a specified number of
		       degrees. If omitted, 0 degrees is assumed.

		       I (for inclination) indicates that the image is to be
		       drawn in perspective as if its upper edge were tilted
		       away from the viewer by a specified number of degrees.
		       If omitted, 0 degrees is assumed.

		       T (for top) indicates that the image is to be drawn
		       beginning at a specified upper row. If omitted, row 1 is
		       assumed.

		       B (for bottom) indicates that the image is to be drawn
		       ending at a specified lower row. If omitted, the
		       overlay's lowest row is assumed.

		       L (for left) indicates that the image is to be drawn
		       beginning at a specified left column. If omitted, column
		       1 is assumed.

		       R (for right) indicates that the image is to be drawn
		       ending at a specified right row. If omitted, the
		       overlay's rightmost column is assumed.

		       V (for viewing distance) indicates that the image is to
		       be drawn in perspective as if seen from a specified
		       number of grid spaces away. If omitted, 100 grid spaces
		       is assumed.

		       F (for focal distance) indicates that the image is to be
		       drawn in perspective as if seen with a picture plane at
		       a distance equal to a specified percentage of viewing
		       distance. If omitted, 80 percent is assumed.

		       E (for exaggeration) indicates that the image is to be
		       drawn in perspective as if its third dimension were to
		       be scaled by a specified percent. If omitted, 100
		       percent is assumed.

		       S (for size) indicates that the image is to be drawn as
		       if it were to be scaled by a specified percent. If
		       omitted, 100 percent is assumed.

		       C (for background color) indicates that the color of the
		       background on which the image is to be drawn should be
		       that associated with a specified value. If omitted, the
		       value 3 is assumed.

		       M (for monitor) indicates that drawing should commence
		       on a color monitor then pause for a carriage return.

		       D (for dump) indicates that drawing should commence on a
		       color monitor and then be screen dumped to a graphics
		       printer.

		       G (for graphics printer) indicates that drawing should
		       commence on a graphics printer.

		       P (for plotter) indicates that drawing should commence
		       on a pen plotter.

		       Q (for quit) indicates that the SHOW operation should be
		       terminated.

		       This letter must be specified at the beginning of a new
		       line of text. If omitted, no drawing control parameter
		       is changed.

       number	       gives a three-digit number setting the most recently
		       specified drawing control parameter. This must follow
		       the letter identifying that parameter with no
		       intervening space. If omitted, 000 is assumed.
