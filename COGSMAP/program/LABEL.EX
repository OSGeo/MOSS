OPERATION LABEL

The LABEL operation reassigns user-specified labels to selected regions of an
input overlay.

The general form of the instruction for this operation is:

       LA[BEL] overlay
       [value space label]
       .
       .
       .

where individual phrases are interpreted as follows.

       LABEL overlay   gives the title of the input overlay.

       value space label
		       gives a two-digit value (with no preceding characters or
		       spaces) followed immediately by a blank space and a
		       label of up to sixteen characters to be used to
		       represent the region associated with that value.

		       The LABEL operation will continue to expect value-
		       space-label input on new lines of text until a value of
		       less than 0 is encountered.
