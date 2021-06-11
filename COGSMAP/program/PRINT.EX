OPERATION PRINT

The PRINT operation reproduces written text at a specified graphic size.  Text
may be reproduced either in the form of normal characters or in the form of
enlarged characters made up of symbols as shown below.

	  ########    ######	  ########    ######	  ########
	  ##	##    ##  ##	  ##	##    ##    ##	  ##
	  ##	##    ########	  ##	      ##    ##	  ######
	  ########    ##    ##	  ##	##    ##    ##	  ##
	  ##	##    ########	  ########    ######	  ########

Each enlarged character is eight spaces wide and some specified multiple of
five lines high.  While any characters available to the input and output
devices at hand may be reproduced at normal size, enlarged output is limited to
letters, numbers, and the following special symbols:

       + (an addition sign)
       - (a subtraction sign)
       / (a slash)
       . (a period)
       ' (a single quote)
       $ (a dollar sign)
       ? (a question mark)
       = (an equal sign)
       # (a pound sign)
       % (a percent sign)
       " (a double quote)
	 (an underline; reproduced as a broken line)
       _
       * (an asterisk; reproduced as a centered dot)

The general form of the instruction for this operation is:

       PRI[NT] [A[T] number]
       text
       .
       .
       .

where individual phrases are interpreted as follows.

       AT number       gives the height of the text to be printed in five-line
		       increments.  If omitted, a height of 1 increment is
		       assumed.  If specified as less than or equal to 0, text
		       is not enlarged.

       text	       is the string of characters to be printed. It may
		       include up to 7 characters if enlarged or up to 80
		       characters if not. Text lines will continue to be read
		       until an all-blank line is encountered.
