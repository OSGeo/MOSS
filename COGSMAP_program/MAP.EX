OPERATION MAP

The MAP operation starts or restarts execution of the Map Analysis Package.
This provides the opportunity to select an existing map to be processed or to
create a new one of specified dimensions. It also provides an opportunity to
copy data from one map to another.

Once a map name has been specified, MAP will search for a DOS file of the same
name with .IN as its file name extension. If such a file exists in the current
directory, a READ operation will automatically be executed on that file.

The general form of the instruction for this operation is:

       MAP [overlay] ...
       [top bottom left right]
       name
       [rows columns scale]
       [top bottom left right]

where individual phrases are interpreted as follows.

       overlay	       is the title of an overlay to be copied to another map.

       top bottom left right
		       gives four numbers, each specified at the beginning of a
		       new line of input text, to indicate respectively the top
		       row, the bottom row, the left column, and the right
		       column of areas from which and to which to copy. The
		       first specification of this phrase identifies the area
		       from which data should be copied, while the second
		       identifies the area to which data should be copied.

		       These phrases are meaningful only for map-to-map
		       copying, in which case they are required.

       name	       gives the name of the map to be processed, specified at
		       the beginning of a new line of input text. If no such
		       map exists, rows-columns-scale input will be prompted in
		       order to create one.

       rows columns scale
		       gives three 3-digit numbers, each specified at the
		       beginning of a new line of input text, to indicate
		       respectively the number of rows, the number of columns,
		       and the standard scale of the map to be created.
