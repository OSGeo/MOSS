OPERATION READ

The READ operation provides for the input of instructions from previously
created DOS text files rather than from the keyboard. Such files would normally
be created in DOS with a text editing program. To be recognized by the READ
operation, they must have DOS file names with the extension ".IN" and be
accessible from the current directory.

The general form of the instruction for this operation is:

       REA[D] name

where:

       READ name       gives the name (without extension) of the DOS file from
		       which MAP instructions are to be read. If no name is
		       specified, control returns to a point just after the
		       previous READ, if any.
