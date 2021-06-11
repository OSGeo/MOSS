OPERATION WRITE

The WRITE operation provides for the direction of text output to a specified
DOS text file rather than to the terminal. This file will be located in the
current directory under a specified file name with ".IN" as its file neame
extension.

The general form of the instruction for this operation is:

       W[RITE] name

where:

       WRITE name      gives the name (without extension) of the DOS file to
		       which subsequent text output is to be written. If no
		       name is specified, subsequent text output is directed to
		       the terminal.
