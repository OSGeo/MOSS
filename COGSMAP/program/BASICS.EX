

























		   THE IBM PERSONAL COMPUTER VERSION OF
			 THE MAP ANALYSIS PACKAGE


			      C. Dana Tomlin
	   Laboratory for Computer Graphics and Spatial Analysis
		     Harvard Graduate School of Design
			 Cambridge, Massachusetts

































CONTENTS
________


Introduction . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .   1

Conventions  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .   2

	Data . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .   2

		Maps . . . . . . . . . . . . . . . . . . . . . . . . . .   3

		Map Names  . . . . . . . . . . . . . . . . . . . . . . .   3

		Map Overlays . . . . . . . . . . . . . . . . . . . . . .   3

		Overlay Titles . . . . . . . . . . . . . . . . . . . . .   3

		Overlay Protection Indicators  . . . . . . . . . . . . .   4

		Overlay Scales . . . . . . . . . . . . . . . . . . . . .   4

		Overlay Regions  . . . . . . . . . . . . . . . . . . . .   4

		Region Values  . . . . . . . . . . . . . . . . . . . . .   4

		Region Labels  . . . . . . . . . . . . . . . . . . . . .   5

		Points	(Rows, Columns, Grid Spaces,
			 Coincident Points, Neighborhoods) . . . . . . .   5

		Point Coordinates  . . . . . . . . . . . . . . . . . . .   5

	Data Processing  . . . . . . . . . . . . . . . . . . . . . . . .   6

		Operations . . . . . . . . . . . . . . . . . . . . . . .   6

		Procedures . . . . . . . . . . . . . . . . . . . . . . .   9

	Data Processing Control  . . . . . . . . . . . . . . . . . . . .  10

		Instructions . . . . . . . . . . . . . . . . . . . . . .  10

		Responses  . . . . . . . . . . . . . . . . . . . . . . .  14

		Programs . . . . . . . . . . . . . . . . . . . . . . . .  15






















Capabilities . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .  16

	Operation ADD  . . . . . . . . . . . . . . . . . . . . . . . . .  21

	Operation ASSIGN . . . . . . . . . . . . . . . . . . . . . . . .  21

	Operation AVERAGE  . . . . . . . . . . . . . . . . . . . . . . .  22

	Operation CLUMP  . . . . . . . . . . . . . . . . . . . . . . . .  23

	Operation COPY . . . . . . . . . . . . . . . . . . . . . . . . .  24

	Operation COVER  . . . . . . . . . . . . . . . . . . . . . . . .  24

	Operation CROSS  . . . . . . . . . . . . . . . . . . . . . . . .  25

	Operation DESCRIBE . . . . . . . . . . . . . . . . . . . . . . .  25

	Operation DIFFERENTIATE  . . . . . . . . . . . . . . . . . . . .  26

	Operation DISPLAY  . . . . . . . . . . . . . . . . . . . . . . .  26

	Operation DIVIDE . . . . . . . . . . . . . . . . . . . . . . . .  28

	Operation DRAIN  . . . . . . . . . . . . . . . . . . . . . . . .  28

	Operation ECHO . . . . . . . . . . . . . . . . . . . . . . . . .  29

	Operation EULER  . . . . . . . . . . . . . . . . . . . . . . . .  29

	Operation EXPLAIN  . . . . . . . . . . . . . . . . . . . . . . .  29

	Operation EXPONENTIATE . . . . . . . . . . . . . . . . . . . . .  30

	Operation EXPOSE . . . . . . . . . . . . . . . . . . . . . . . .  30

	Operation GRID . . . . . . . . . . . . . . . . . . . . . . . . .  31

	Operation LABEL  . . . . . . . . . . . . . . . . . . . . . . . .  32

	Operation LIST . . . . . . . . . . . . . . . . . . . . . . . . .  32

	Operation MAP  . . . . . . . . . . . . . . . . . . . . . . . . .  33

	Operation MAXIMIZE . . . . . . . . . . . . . . . . . . . . . . .  34

	Operation MINIMIZE . . . . . . . . . . . . . . . . . . . . . . .  34

	Operation MULTIPLY . . . . . . . . . . . . . . . . . . . . . . .  35


















	Operation NOTE . . . . . . . . . . . . . . . . . . . . . . . . .  35

	Operation ORIENT . . . . . . . . . . . . . . . . . . . . . . . .  36

	Operation POINT  . . . . . . . . . . . . . . . . . . . . . . . .  37

	Operation PRINT  . . . . . . . . . . . . . . . . . . . . . . . .  38

	Operation PROFILE  . . . . . . . . . . . . . . . . . . . . . . .  39

	Operation PROTECT  . . . . . . . . . . . . . . . . . . . . . . .  40

	Operation QUIET  . . . . . . . . . . . . . . . . . . . . . . . .  41

	Operation RADIATE  . . . . . . . . . . . . . . . . . . . . . . .  41

	Operation READ . . . . . . . . . . . . . . . . . . . . . . . . .  43

	Operation RECODE . . . . . . . . . . . . . . . . . . . . . . . .  43

	Operation RESPACE  . . . . . . . . . . . . . . . . . . . . . . .  44

	Operation SCALE  . . . . . . . . . . . . . . . . . . . . . . . .  45

	Operation SCAN . . . . . . . . . . . . . . . . . . . . . . . . .  46

	Operation SCORE  . . . . . . . . . . . . . . . . . . . . . . . .  49

	Operation SHOW . . . . . . . . . . . . . . . . . . . . . . . . .  51

	Operation SIZE . . . . . . . . . . . . . . . . . . . . . . . . .  53

	Operation SLICE  . . . . . . . . . . . . . . . . . . . . . . . .  53

	Operation SORT . . . . . . . . . . . . . . . . . . . . . . . . .  54

	Operation SPAN . . . . . . . . . . . . . . . . . . . . . . . . .  54

	Operation SPREAD . . . . . . . . . . . . . . . . . . . . . . . .  55

	Operation STOP . . . . . . . . . . . . . . . . . . . . . . . . .  56

	Operation STRIP  . . . . . . . . . . . . . . . . . . . . . . . .  56

	Operation SUBTRACT . . . . . . . . . . . . . . . . . . . . . . .  57

	Operation SURVEY . . . . . . . . . . . . . . . . . . . . . . . .  57

	Operation TRACE  . . . . . . . . . . . . . . . . . . . . . . . .  58

	Operation WRITE  . . . . . . . . . . . . . . . . . . . . . . . .  59

	Operation ZAP  . . . . . . . . . . . . . . . . . . . . . . . . .  59














INTRODUCTION
____________


The  Map  Analysis Package (MAP) is a set of computer programs that provide for
the  storage,  manipulation,  and  display  of	cartographic  data.   Initially
developed  as  a  student project at Harvard University (Tomlin 1975) and later
expanded  as  part  of	a  doctoral dissertation at Yale (Tomlin 1983), MAP has
evolved  in conjunction with an ongoing effort to generalize and to standardize
methods of processing cartographic information.

Use  of  the  Map  Analysis  Package  is  in  many  ways  similar to the use of
traditional  techniques involving conventional geographic maps.  Its processing
capabilities  are  organized  as  a  series  of  basic	operations  that can be
flexibly  combined into a variety of more complex procedures.  These operations
are  specified	and  controlled  through  a  user-oriented  command language of
English-like phrases.

This  manual  describes  the  conventions  and capabilities of the Map Analysis
Package  as  implemented  for  use  with the Disk Operating System (DOS) on the
International Business Machines Corporation (IBM) Personal Computer.





























		       ________________________________


Tomlin, C.D. 1975. The Tomlin Subsystem of IMGRID. Unpublished manuscript.
		   Department of Landscape Architecture. Harvard Graduate
		   School of Design.

	     1983. Digital Cartographic Modeling Techniques in Environmental
____________
		   Planning. Doctoral dissertation. Division of Forestry and
		   Environmental Studies. Yale Graduate School.









CONVENTIONS
___________


Like  most  data-processing  systems, the Map Analysis Package employs specific
conventions regarding:

	 * data;

	 * the way in which data are processed; and

	 * the way in which processing is controlled.


DATA

The  major  components	of MAP's data structure correspond to familiar elements
of  traditional cartographic representation.  They are organized hierarchically
as follows:

	 * Map

	    * Map Name

	    * Map Overlay

		    * Overlay Title

		    * Overlay Protection Indicator

		    * Overlay Scale

		    * Overlay region

			    * Regional Value

			    * Regional Label

			    * Point

				    * Point Coordinate

				    * Point Coordinate

			    * (Additional Points)

		    * (Additional Regions)

	    * (Additional Overlays)

    * (Additional Maps)

















Maps
____

At  the  highest level in this hierarchical structure is the map.  A map is the
set  of all cartographic data for a particular geographic area.  In most cases,
a  map	will  initially contain information characterizing each location within
the  area  in  terms  of  its  topography,  demographics,  land use, and so on.
Eventually,  it  will  also  contain  any  new	information  generated from the
initial data.

The  major  difference between this type of map and its traditional counterpart
relates  to  the  way in which information is organized.  On a traditional map,
different  geographic characteristics such as elevation and land use might well
all  appear  on  one  graphic  image.  Here, each characteristic is represented
alone on a separate map overlay.


Map Names
_________

Maps  are  referred  to by names that are specified as each new map is created.
The  name  of  a  map must be comprised of one or two letters, numerals, and/or
selected  characters  including (in DOS Version 2.1) $, &, #, @, !, %, `, `, (,
),  -,	{, },  , /, and \. Typical of valid map names, for example, are BP, #2,
	      _
and !. No two maps in the same DOS directory may share a common name.


Map Overlays
____________

A  map	overlay  of  a	geographic  area is the set of cartographic information
pertaining  to	just  one  of that area's characteristics.  It is, in effect, a
single-factor  map.  In contrast to the multiple-factor map described above, an
overlay  characterizes	each  location	within	a  geographic  are  in terms of
exactly one attribute.

A  map	will  normally	contain several dozen overlays, and all will usually be
registered  with  respect  to  the  same cartographic base.  A typical map will
initially  include  overlays  associated  with	features such as bedrock, soil,
landforms,  water  bodies, vegetation, ownerships, roads, buildings, and so on.
These  overlays  will  normally  be  generated directly from conventional maps,
aerial	photographs,  or  site	investigations.  Eventually, a typical map will
also  include  additional  overlays  that  have  been  computationally derived.
These  will  portray characteristics such as topographic slope and aspect, soil
suitability  for a prospective land use, the visibility of a water feature, the
size  or  shape  of  openings  in a forest canopy, distance to the neaest road,
density  of housing, and so on.  Almost all of MAP's data processing is done on
an overlay-by-overlay basis.

The  pieces  of information comprising each overlay of a map include its title,
its protection indicator, its scale, and a set of one or more regions.


Overlay Titles
______________

Overlays  are  referred  to by titles that are specified as each new overlay is
created.  The  title  of  an  overlay must be an unbroken string of from one to
eight  letters,  numerals, and/or selected characters including (in DOS Version
2.1)  $,  &,  #,  @, !, %, `, ', (, ), -, {, },  , /, and \. No two overlays of
						_
the  same  map may share a common title, however, and reserved DOS device names
(including  AUX,  COM1,  COM2,	CON,  LPT1,  LPT2,  LPT3, NUL, and PRN) must be
avoided. Typical of valid overlay titles are LAND-USE, #9/18/50, and X.








Overlay Protection Indicators
_____________________________

An  overlay's  protection  indicator  is a piece of information that determines
whether  or  not  the  overlay	is  able  to  modified	or  deleted. Protection
indicators  characterize  overlays as either protected or exposed.  A protected
overlay  can  be  used	in  processing	but  only  if it remains unchanged as a
result.  This  is  in  contrast  to  an exposed overlay, which can be modified,
replaced,   or	 completely   erased  through  routine	processing  techniques.
Overlays  are automatically exposed when created, but protection indicators can
be changed at any time.


Overlay Scales
______________

The  scale  of	an  overlay  is  a  number  describing the relationship between
distance  as  measured "on the ground" and distance as measured "on paper."  It
is  an	integer  which	specifies the number of feet, meters, or other units of
gegographic  distance that are to be equated with a single unit of cartographic
distance.    (Units  of  cartographic  distance are more fully described in the
Points	section  below.)   While this relationship does not affect the physical
______
dimensions   of   graphic   output,   it  does	affect	computations  involving
measurement  in  three	dimensions.   A standard scale, specified when a map is
initially  established,  is automatically associated with each overlay as it is
created.  Scales, however, can be changed at any time.


Overlay Regions
_______________

The  regions  of  an  overlay  correspond  to  the  various  site conditions or
features  that characterize specific locations.  An overlay entitled WATER, for
example,  might  include  regions  respectively associated with streams, lakes,
rivers,  ocean,  and  (since  all  locations  must  be accounted for) dry land.
While  most  overlays can be expected to have fewer than a dozen  regions, this
number is highly variable.

The  information associated with each region of an overlay includes a numerical
value, a label, and a set of one or more points.


Region Values
_____________

The  value  of	a  region  is  an  integer  ranging  from  -32000 to 32000 that
distinguishes  the  region from others of the same overlay and enables it to be
processed  numerically.    On a WATER overlay, for example, the dry land region
might  be  represented	by  a value of 0, while streams are represented by a 1,
lakes  by  2,  and  so on.  In this case, the values would have no quantitative
significance,  but  merely  serve  as  qualitative  identifiers. On overlays of
characteristics  such  as  topographic	elevation or land cost, however, region
values	might  well  relate  to  quantitative units of measurement such as feet
above sea level or dollars per acre.

Values	are explicitly assigned to regions when overlays are initially created.
They   can  also  be  generated  computationally  as  subsequent  overlays  are
derived.    If values to be assigned or generated are not in integer form, they
will  automatically  be  rounded off to the nearest integer or truncated to the
next  smaller  integer	before	being stored.  If they are above 32000 or below
-32000, they will usually result in errors.










Region Labels
_____________

Regions   represented  by  values  ranging  from  0  through  99  can  also  be
represented  by  labels.    A  label  is  a  sequence of from one to sixteen of
letters,  numerals, characters and/or blank spaces such as DRY LAND, CAMBRIDGE,
or  $1,000  /  HECTARE.  Labels  are  automatically  set to		     (a
							    ________________
sequence  of  sixteen  underline  characters)  when  an  overlay  is  initially
created,  but may then be redefined at any time.  Unlike the names of maps in a
common	DOS  directory	or  the  titles of overlays in the same map, the labels
assigned to an overlay's regions may  duplicate one another.


Points (Rows, Columns, Grid Spaces, Coincident Points and Neighborhoods)
________________________________________________________________________

The  points  of  a  region are square subdivisions of an overlay's cartographic
area.	 The overall area represented by an overlay must be rectangular, and it
must  have  designated	upper, lower, right and left edges. These will usually,
though	not  necessarily,  correspond to the north, south, east, and west edges
of  the  geographic  area  being  mapped.   The points within this cartographic
rectangle  are	arranged  in  a  regular  grid	pattern  of equally spaced rows
(parallel  to  the  rectangle's upper and lower edges) and columns (parallel to
its  left  and	right  sides).	The  cartographic  dimension between centers of
adjacent  rows	of columns is referred to as one grid space, and the geographic
distance represented by this dimension is recorded as an overlay's scale.

Every  point  of  an  overlay  must  be  associated  with  exactly  one of that
overlay's  regions.   This is true even if the geographic area represented by a
point  is  characterized  only by the absence of any particular feature such as
the  dry  land	on  an overlay of water bodies. It is also true when an area is
occupied  by  more  than  one feature such as dry land, a stream, and part of a
lake.	 In  this  case,  a decision must be made as to which feature should be
recorded or how the joint occurance should be represented as a single region.

All  overlays of the same map share the same rectangular pattern of points, the
numbers  of  rows  and	columns of which are specified when the map is created.
The  pattern  normally	relates  to one geographic area such that points at the
same   row-column  position  on  different  overlays  correspond  to  the  same
geographic  location.  Such  points  are  referred to as coincident. The set of
points	that lie within a specified distance and/or direction of any particular
point on an overlay are referred to as that point's neighborhood.

Point Coordinates
_________________

The  relative cartographic location of each point on an overlay is defined by a
pair  of  Cartesian  coordinates  indicating its row and column position. These
coordinates  are  expressed as consecutive integers beginning with row 1 and at
the top edge and column 1 at the left edge of the overlay's rectangular area.




















DATA PROCESSING

The  data  processing  conventions  employed  by  the  Map Analysis Package are
comparable  to	those associated with manual techniques for the manipulation of
cartographic overlays.	All data processing capabilities are provided through:

     * operations; and

     * procedures.


Operations
__________

Operations  are  individual  data  processing  activities.    Each operation is
identified  by a name in the form of an imperative verb intended to suggest its
processing  function.  The Map Analysis Package includes several dozen of these
operations,  and  each	is  associated with one of four major types of function
including:

     * processing control;

     * data input;

     * data output; and

     * data transformation.

Processing control operations affect interaction between digital input
or  output  devices  and  the  processing  unit  of  a	computer.  They include
operations:

     * MAP, which starts or restarts the Map Analysis Package,

     * STOP, which terminates its execution; and

     * READ and WRITE, which provide for input and output by way of
       disk files.

Data  input  operations  control  the  flow of information from an input device
through  the  computer's  processing unit to its digital storage device.  These
include operations:

     * POINT, which records information by cartographic point;

     * TRACE, which records information by line;

     * GRID, MAP, and STRIP, which do so by area; and

     * ZAP, which deletes information.


















Data  output  operations  provide  for	the display of information.  Among them
are:

    * LIST, which identifies each of a map's overlays;

    * DESCRIBE, DISPLAY, SHOW, and CONTOUR, which display the information
      associated with a single overlay;

    * SCORE, which compares one overlay to another by way of statistical
      tables;

    * NOTE and PRINT, which provide for annotation of input and output;

    * ECHO and QUIET, which control the issuing of responses to
      instructions; and

    * EXPLAIN, which provides reference information.

The  largest  group  of  MAP operations is associated with data transformation.
All  data transformation is done on an overlay-by-overlay basis.  Some of these
operations modify existing overlays while others generate new ones.

Among the data-transforming operations that modify existing overlays are:

    * LABEL, which modifies regional labels;

    * PROTECT and EXPOSE, which alter an overlay's protection status; and

    * SCALE, which specifies its scale.

Data  transformation  operations  that generate new overlays do so by accepting
one  or  more overlays as input in order to produce a single overlay as output.
In  referring to overlays in this context, the former are termed input overlays
and  the  latter  output  overlays  with  respect  to  the  operation involved.
Similarly,  the  values associated with these overlays are referred to as input
values and output values, respectively.  Each overlay-generating operation:

    * copies one or more input overlays from the storage device to the
      processing unit; then

    * transforms the copied overlay(s) to generate an output overlay; and

    * copies the output overlay back to the storage device.

In  copying input overlays from storage device to processing unit, the original
versions  on  the  storage  device  remain  intact.    Any overlays left in the
processing  unit  from	a  previous  operation, however, are overwritten in the
process.    When  an  output overlay is copied from the processing unit back to
the  storage device, it is directed to a DOS file selected according to the new
overlay's  title.    If  the  title  matches  that of an  existing overlay, the
existing  overlay  is overwritten unless it's protection status precludes this,
in  which  case  an error is detected.	If the new title does not match that of
an existing overlay, the output overlay is directed to a new DOS file.














Data-transforming  operations  that  generate new overlays can be characterized
according  to  whether the new value assigned to a typical point is computed as
a function of existing values associated with:

    * the point itself;

    * all points within a certain neighborhood of that point; or

    * all points in a similar region.

Among the operations that compute new values on a point-by-point basis are:

    * RECODE and CROSS, which substitute new values for existing values
      according to explicit specification;

    * ADD, SUBTRACT, MULTIPLY, DIVIDE, EXPONENTIATE, MAXIMIZE, MINIMIZE,
      and AVERAGE, which combine two or more values of each point
      mathematically;

    * COVER, which superimposes portions of one overlay onto another; and

    * COPY, which reproduces an overlay under a new title.

Operations  that  compute new values as functions of cartographic neighborhoods
include:

    * SCAN, which combines the values of the points within each
      neighborhood by way of mathematical functions;

    * DIFFERENTIATE, ORIENT, and PROFILE, which measure surficial slope,
      aspect and shape characteristics from values representing elevation;

    * SPREAD, which measures proximity;

    * SPAN, which measures narrowness;

    * RADIATE, which delineates viewsheds; and

    * DRAIN, which simulates surficial flow.

 Region-characterizing operations include:

    * SCORE, which computes a new value for each point as a mathematical
      function of co-regional values;

    * SIZE, which measures the area of regions;

    * EULER and SURVEY, which characterize regions according to shape; and

    * SLICE and SORT, which regard an entire overlay as one region and
      standardize the range of values within it.
















Procedures
__________

Individual  operations	are associated with specific functions.  Operations can
be  combined, however, to provide an open-ended range of capabilities.	This is
done  by controlling the order in which operations are applied to a common body
of data.

Any  finite  sequence  of  two	or  more  operations  applied in this manner to
meaningful data with conscious intent constitute what is termed a procedure.

In  a  procedure,  data  transformation  operations  are  combined by using the
output	of  one  as input to another. In this way, what are termed intermediate
overlays are used to store temporary results for subsequent processing.

Most  procedures  embody a structure in which two or more overlays are accepted
as  input  by  way  of	two  or  more  operations  and	are then processed over
independent  sequences	of  subsequent	operations which eventually converge to
generate  a  single  overlay as output.  This structure can be represented as a
hierarchy of overlays in which:

    * the root of the hierarchical tree corresponds to the output overlay;

    * its leaves correspond to the input overlays; and

    * each overlay created is related to its immediate progenitor(s) by
      way of a single processing operation.









































DATA PROCESSING CONTROL


The  Map  Analysis  Package  is  invoked from DOS by the batch command MAP. Its
data  processing capabilities are then controlled through a command language of
English-like  text.  This  language  enables a user to control the execution of
individual  operations,  the  data  to which they are applied, and the order in
which they are performed.  Its major elements include:

    * instructions;

    * responses; and

    * programs.

Instructions
____________

Each  Map Analysis Package operation is invoked by a different instruction.  An
instruction  is  a  sequence of letters, numerals, symbols, and/or blank spaces
as illustrated below.

    STOP

    EXPLAIN STOP

    ORIENT THAT   PRECISELY

    SPREAD THAT   TO 25

    CROSS THIS	 WITH THAT   FOR THEM	/
    ASSIGNING 1   TO 2 3   AND 4   TO 5 6

Each  instruction  is  specified  on one or more 80-character lines of text and
may  be entered from a keyboard or disk file.  If entered from a keyboard, each
line  of an instruction must be typed and then sent to the computer by pressing
the  ENTER  key.    To	be  entered  from  a disk file, each line must first be
recorded in that file through some sort of text editing program.

Most  instructions  must  be  specified  on  a	single	line  of  text.   These
instructions  are  implicitly  assumed to end whenever a new line begins.  Some
instructions  can  be  extended  to  two or more lines, however, by ending each
line  prior to the final one with a designated symbol (/).  This is true of the
CROSS  instruction  illustrated above.	Other instructions are assumed to go on
until  an ending is explicitly marked.	This is most often true of instructions
for data input operations.

On  each  line	of  most  instructions,  strings  of  one  or more consecutive,
non-blank  characters  are  distinguished from one another by intervening blank
spaces.    The	number	of  blanks  before, between, or after these strings may
vary.	 No  extraneous strings should be included, however, since each will be
regarded as significant.
















Strings  of  non-blank	characters  in	an instruction form words, numbers, and
marks  of punctuation much as they would in English.  Here, however, vocabulary
and syntax must conform to special rules.

The  first string of non-blank characters in an instruction must match the name
of  an available operation.  If it does, the instruction is interpreted further
according  to  the syntax for that operation.  If it does not, this is regarded
as  an	error.	  The  names  of  the  operations  invoked  by the illustrative
instructions cited above are underlined below.

     STOP
     ____

     EXPLAIN STOP
     _______

     ORIENT THAT   PRECISELY
     ______

     SPREAD THAT   TO 25
     ______

     CROSS THIS   WITH THAT   FOR THEM	 /
     _____
     ASSIGNING 1   TO 2 3   AND 4   TO 5 6

Instructions  may  also  include  modifiers.  These correspond in both form and
function  to prepositions, adverbs, adjectives, nouns, and marks of punctuation
in  English.	The  modifiers	in  the  set  of  instructions	cited above are
underlined below.

     STOP

     EXPLAIN STOP

     ORIENT THAT   PRECISELY
		   _________

     SPREAD THAT   TO 25
		   __

     CROSS THIS   WITH THAT   FOR THEM	 /
		  ____	      ___
     ASSIGNING 1   TO 2 3   AND 4   TO 5 6
     _________	   __	    ___     __

Each  operation's  instruction  recognizes  only certain modifiers as valid and
regards all others as errors.  Neither

     ORIENT THAT   EXACTLY
		   _______

nor

     SPREAD THAT   UNTIL 25
		   _____

for  example,  would  be  accepted  with modifiers as underlined.  Furthermore,
each  operation  interprets  its  modifiers  in  its  own  particular way.  The
modifier TO, for example, is interpreted differently by RENAME and CROSS.


















The  initial  imperative  verb	(i.e.  operation  name) and the modifiers of an
instruction  may  also	take  on objects.  Most often, these are in the form of
overlay  titles  or  numbers.	The objects in the instructions cited above are
underlined below:

     STOP

     EXPLAIN STOP
	     ____

     ORIENT THAT   PRECISELY
	    ____

     SPREAD THAT   TO 25
	    ____      __

     CROSS THIS   WITH THAT   FOR THEM	 /
	   ____        ____	  ____
     ASSIGNING 1   TO 2 3   AND 4   TO 5 6
	       _      ___	_      ___

When  a  verb  or  modifier  does  call  for one or more objects, note that the
object(s)  must  be  specified	immediately  following	the  verb  or  modifier
involved.    Each  object must be of the specific type designated for that verb
or  modifier, or it will be treated as an error.  The modifier TO in the SPREAD
instruction,  for  example, must be followed by a single number, whereas the TO
modifier in the CROSS instruction must be followed by a pair.

When  an  instruction  includes two or more modifiers and/or objects, the order
in  which  these are specified is generally significant only to the extent that
it  would appear so in English.  Consider, for example, the apparent difference
between

     CROSS THIS   WITH THAT   FOR THEM	 /
     ASSIGNING 1   TO 2 3   AND 4   TO 5 6

and

     CROSS THIS   WITH THAT   FOR THEM	 /
     ASSIGNING 1   TO 5 6   AND 4   TO 2 3

Often,	the  specification  of	a  modifier  and  its  object(s),  if  any,  is
optional.   The omission of an optional modifying phrase usually means that the
option	simply	does  not take effect.	This would be the case, for example, if
the  TO  2  3  phrase  were to be removed from the CROSS instruction above.  In
some  cases, however, the omission of an optional modifying phrase means that a
standard  option  is  to be assumed by default.  If FOR THEM were to be removed
from  the  CROSS  instruction,	for example, it would be replaced implicitly by
FOR X.

The   verb,   modifiers   and  objects	of  an	instruction  can  sometimes  be
abbreviated.	This can usually be done by specifying just enough of the first
few  characters of an entry to distinguish from valid alternatives.  The string
EXPL,  for  example, would be sufficient to call for operation EXPLAIN since no
other  operation  name	begins	with  that four letter sequence.  EXP, however,
would not be sufficient to distinguish EXPLAIN from EXPOSE or EXPONENTIATE.
















Once  the  distinguishing  characters of a verb or modifier are understood, any
remaining   characters	 in   the  string  are	ignored.    These  may	include
misspellings,  marks  of  punctuation,	parenthetical  notes, and so on without
misinterpretation.    It  is  because  of this that the string AND in the CROSS
instruction  cited  above  can	be  regarded  as  equivalent  to  the  modifier
ASSIGNING.   It is also because of this that the following instructions can all
be regarded as equivalent.

     EXPLAIN  STOP

     EXPL  STOP

     EXPLANE  STOP

     EXPLICATE	STOP

     EXPLAIN,-IF-YOU-WILL,-THE-MEANING-OF-THE-OPERATION-CALLED	STOP

The  same  is also true of objects.  In the case of numbers, however, it is the
first  non-numerical  character that marks the end of the significant input and
the beginning that which will be ignored. The number

     12

     for example, could also be given as

     12$

or

     12-DOLLARS

In  the  case  of  overlay  titles, it is only after the first eight characters
have  been  specified  that  subsequent characters are ignored.  Thus, only the
first three of the following overlay titles would be regarded as equivalent.

     VEGETATION.OVERLAY

     VEGETATION

     VEGETATI

     VEGGIES

     VEG






















Responses
_________

As  instructions  are  processed  by  the Map Analysis Package, the system will
respond  with  various	forms of text that can be directed either to a terminal
or  to	a  disk file.  These responses include prompts, error messages, echoes,
and comments.

When  MAP is ready to accept an instruction, it will issue a prompt in the form
of  a  single  period (.) at the beginning of the next line of output text.  It
will  then  read the first line of whatever is entered and check this input for
errors.   If an error is detected, a descriptive message beginning "NO" will be
issued, and the rest of that input line will be disregarded.

If  no	errors	are  detected,	however,  the system will either issue a single
comma  (,)  to	call  for the instruction's next line or, if this was the final
line,	it   will   perform   the   invoked   operation.  At  this  point,  any
so-far-undetected   error   will  cause  program  execution  to  be  terminated
temporarily and then restarted.

When  executed	successfully,  some  operations will generate their own special
forms	of   additional   text	or  graphic  output.	Most,  however,  unless
deliberately  kept  from  doing  so,  will  merely echo their instructions with
messages beginning "OK."

Operations  may  also  generate  parenthetical comments.  These cite conditions
that are not necessarily critical but which may be of concern.

To  illustrate,  consider  the	following  dialogue of typical instructions and
reponses.

     .

     ADD THIS

     NO, A SECOND INPUT OVERLAY MUST BE SPECIFIED

     .

     ADD THIS TO THAT /

     ,

     FOR THOSE

     (OVERWRITING "THOSE")

     OK, "THOSE" CREATED BY ADDING "THIS"
				TO "THAT"

     .

















Programs
________

Programs  are  the notational units associated with procedures. In other words,
a  program  is	a  sequence  of  instructions.	 It is specified such that each
instruction  begins  on  the  first  non-blank line of text below the preceding
instruction  (if  any)	in  that sequence.  Blank lines of text can be inserted
between  instructions  much  the  same	as  blank spaces can be inserted within
them.	 The  order  in which instructions are specified indicates the order in
which corresponding operations are to be performed.

A brief but otherwise typical MAP program is given below.

     SUBTRACT NEW   MINUS OLD	FOR DIFFERENCE

     MULTIPLY DIFFERENCE   BY 100   FOR DIFFERENCE

     DIVIDE DIFFERENCE	 BY OLD   FOR %CHANGE


















































CAPABILITIES
____________


The  data  processing  capabilities of the Map Analysis Package are embodied in
the  functions	performed  by  individual operations and the way in which these
functions  are	combined. As indicated earlier, MAP's operations are associated
with four major types of activity as follows.

     * Processing control operations include

	 MAP, READ, STOP, and WRITE.


     * Data input operations include

	 ASSIGN, GRID, POINT, STRIP, TRACE, and ZAP.


     * Data output operations include

	 CONTOUR, DESCRIBE, DISPLAY, ECHO, EXPLAIN,
	 LIST, NOTE, PRINT, SCORE, and QUIET.


     * And data transformation operations include

	    * those which modify existing overlays

	 EXPOSE, LABEL, PROTECT, and SCALE; as well as

	    * those which generate new overlays, including

		   * point-characterizing operations

	 ADD,  AVERAGE,  COPY,	COVER,	CROSS,	DIVIDE, EXPONENTIATE, MAXIMIZE,
	 MINIMIZE, MULTIPLY, RECODE, and SUBTRACT;

		   * neighborhood-characterizing operations

	 DIFFERENTIATE,  DRAIN,  ORIENT,  PROFILE,  RADIATE,  SCAN,  SPAN,  and
	 SPREAD; and

		   * region-characterizing operations

	 EULER, SCORE, SIZE, SLICE, SORT, and SURVEY.





















Descriptions  of individual operations are presented below in alphabetical order
by  operation name. Each describes an operation's function, outlines the general
form  of  its  instruction,  explains  available  processing options, and offers
examples  of  use.    The  descriptions  rely  heavily	on  terms  and	concepts
introduced above.

In  describing	the  generic  form  of	each  operation's  instruction,  certain
notational  conventions  are  adopted to represent variable input. The syntax of
the AVERAGE instruction, for example, is expressed diagrammatically as follows:

       AV[ERAGE] overlay   [T[IMES] weight]   P[LUS] overlay   [/]
       [T[IMES] weight]   [P[LUS] overlay   [T[IMES] weight]]...   [/]
       [F[OR] overlay]

Here,  all  items  in  lowercase  letters  signify entries to be supplied by the
user.  The  lowercase items themselves are not to be entered as shown but are to
be  replaced  with actual overlay titles, numbers, or other forms of input. Each
lowercase  term  itself  is intended only to suggest the type of input required.
The  term  "overlay,"  for  example,  will  be  consistently used to represent a
user-supplied title.

The  uppercase	letters  and  any  symbols other than the square brackets in the
syntax	diagram  are to be entered exactly as shown.  Their spacing on each line
of  text  can  vary,  however,	as  long  as each consecutive string of nonblank
characters  remains  intact  and  is separated from others by at least one blank
space.				The	       order		in	      wh









































       AD[D] overlay   T[O] overlay   [T[O] overlay]...   [/]
       [F[OR] overlay]

       AS[SIGN] number	 [F[OR] overlay]

       AV[ERAGE] overlay   [T[IMES] weight]   P[LUS] overlay   [/]
       [T[IMES] weight]   [P[LUS] overlay   [T[IMES] weight]]...   [/]
       [F[OR] overlay]

       CL[UMP] overlay	[A[T] distance]  [D[IAGONALLY]]  [F[OR] overlay]

       CON[TOUR] overlay
       [[letter] [number]]
       .
       .
       .

       COP[Y] overlay	[F[OR] overlay]

       COV[ER] overlay	 W[ITH] overlay   [W[ITH] overlay]...	[/]
       [F[OR] overlay]

       CR[OSS] overlay	 W[ITH] overlay   [F[OR] overlay]   [/]
       [A[SSIGNING] value   [T[O] value value]...   [TH[ROUGH] value]]...

       DE[SCRIBE] overlay

       DIF[FERENTIATE] overlay	 M[AXIMALLY]   [F[OR] overlay]

       DIS[PLAY] overlay   [A[BOVE] row]   [B[ELOW] row]   [/]
       [F[ROM] column]	 [T[HROUGH] column]   [O[VERPRINTING]	[/]
       [S[PECIALLY]]
       [value space characters]
       .
       .
       .

       DIV[IDE] overlay   B[Y] overlay	 [B[Y] overlay]...   [/]
       [F[OR] overlay]

       DR[AIN] overlay	 O[VER] overlay   [F[OR] overlay]

       EC[HO]

       EU[LER] overlay	 [F[OR] overlay]

       EXPL[AIN] name

       EXPON[ENTIATE] overlay	B[Y] overlay   [B[Y] overlay]...   [/]
       [F[OR] overlay]

       EXPOS[E] overlay















       G[RID]	[O[N] overlay]	 [F[OR] overlay]
       format
       row values
       .
       .
       .

       LA[BEL] overlay
       [value space label]
       .
       .
       .

       LI[ST]

       MAP
       name
       [rows
       columns
       scale]

       MAX[IMIZE] overlay   V[ERSUS] overlay   [V[ERSUS] overlay]...   [/]
       [F[OR] overlay]

       MIN[IMIZE] overlay   V[ERSUS] overlay   [V[ERSUS] overlay]...   [/]
       [F[OR] overlay]

       MU[LTIPLY] overlay   B[Y] overlay   [B[Y] overlay]...   [/]
       [F[OR] overlay]

       N[OTE] [text]

       OR[IENT] overlay   [P[RECISELY]]   [F[OR] overlay]

       PO[INT]	 [O[N] overlay]   [F[OR] overlay]
       format
       row column value
       .
       .
       .

       PRI[NT] [A[T] number]
       text
       .
       .
       .

       PRO[FILE] overlay I[NFLECTION] [direction] [A[T] rise] [F[OR] overlay]

       PROT[ECT] overlay

       Q[UIET]

       RA[DIATE] overlay   [A[T] height]   [T[O] distance]   [/]
       [O[VER] overlay]   [ON[TO] overlay]   [TH[ROUGH] overlay]   [/]
       [S[CREENING] diameter]	[AB[OVE] angle]   [F[OR] overlay]

       REA[D] name









       RECO[DE] overlay   [F[OR] overlay]   [/]
       [A[SSIGNING] value   [T[O] value]...   [TH[ROUGH] value]]...

       SCA[LE] overlay	 A[T] number

       SCAN overlay   [statistic]   [A[ROUND] overlay]	 [/]
       [W[ITHIN] diameter]   [D[IAGONALLY]]   [B[EYOND] diameter]   [/]
       [F[OR] overlay]

       SCORE overlay B[Y] overlay [statistic] [Q[UIETLY] [F[OR] overlay]

       SH[OW] overlay	[O[N] overlay]
       [[letter] [number]]
       .
       .
       .

       SI[ZE] overlay	[F[OR] overlay]

       SL[ICE] overlay	 [I[NTO] intervals]   [FR[OM] value]
       [TH[ROUGH] value]   [F[OR] overlay]

       SO[RT] overlay	[F[OR] overlay]

       SPA[N] overlay  [A[ROUND] overlay]  [T[O] width]  [F[OR] overlay]

       SPR[EAD] overlay   [T[O] distance]   [O[VER] overlay]   [D[OWNHILL]
       [TH[ROUGH] overlay]   [F[OR] overlay]

       STO[P]

       STRI[P] overlay	 [O[N] overlay]   [F[OR] overlay]
       [row [value column]...]
       .
       .
       .

       SUB[TRACT] overlay   F[ROM] overlay   [F[ROM] overlay]...   [/]
       [F[OR] overlay]

       SUR[VEY] overlay   [F[OR] overlay]

       T[RACE]	 [A[T] number]	 [T[ARGETTING] x y]   [F[OR] overlay]
       format
       [x y value]
       .
       .
       .

       W[RITE] name

       ZAP overlay















