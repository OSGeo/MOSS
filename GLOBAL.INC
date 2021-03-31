C * Global.Cmn:  Holds global variables in effect throughout the entire system.

C# Bring 1st page to 416 words.
	Integer	IDummy
	Common / Dumb / IDummy


C * Common / GISLog / holds those items that equal .TRUE. [1] or .FALSE. [0].

C# Error exits.
	Logical Error
C# Leave module/program.
	Logical Exit
C# HELP requested.
	Logical Help
C# Prompt type.
	Logical Display
	Common / GISLog / Error, Exit, Help, Display


C * Common / Terminal /: items that may be used to define interface mode.

C# Current receive rate.
     	Integer BaudR
C# Character set:  0=ASCII; 1=EBCDIC.
	Integer ChrSet
C# Echo status:  0=On/Full duplex; 1=Off/Half duplex.
	Integer Echo
	Integer Duplex
	Common / Terminal / BaudR, ChrSet, Echo
	Equivalence ( Duplex, Echo )


C * Common / MenuSel /:  external commands available to the system.  These
C   commands should always call another program (refer to "<program name>.CMD".

C# Matching commands.
	Integer AlfCmd(25)
C# Number of imbedded commands.
	Integer IONum
C# Command requested.
	Integer PrgRun
	Common / MenuSel / AlfCmd, IONum, PrgRun
C * Common / GISIO / contains some nice variables that can be used to open the
C   various IO files that GIS uses.  If you want to compile GIS on a system
C   that does not use the standard channels for I/O, the proper channels for
C   the operating system can be changed in GISInitVar.

C# Subroutine access open/close.
	Integer SrchIO
C# User input interface channel.
	Integer UsrIn
C# User output interface channel.
	Integer UsrOut
	Common / GISIO / SrchIO, UsrIn, UsrOut


C * Common / GTime / contains some variables for finding out what time it is,
C   how long you've been working, etc.

C# Current Date/Time.
	Integer CDate(3), CTime(3)
C# Initial Date/Time.
	Integer IDate(3), ITime(3)
	Common / GTime / CDate, CTime, IDate, ITime


C * Common / MisIVar / and / MiscRVar / contain some useful vairables
C   for doing useless things.  Note:  Common / MiscCVar / is
C   primarily for equivalencing the command input area.

C# Starting position for main command.
	Integer FrsChr
C# Error indicator.
	Integer IEr
C# Non-program generated commands.
	Integer NulCmd
C# Program generated commands.
	Integer NumCmd
C# Index variable for returns.
	Integer RetLbl
C# Starting position for sub command.
	Integer SecChr
	Common / MiscIVar / FrsChr, IEr, NulCmd,
     +		NumCmd, RetLbl, SecChr


C# Current version of GIS.
	Real	Vrsion
	Common / MiscRVar / Vrsion


C# Area for file name.
	Character*40 FileNm
C# Program identifier.
	Character*80 WhoIAm
C# User input area.
	Character*80 CmdStr
C# Hold Area.
	Character*12 Cmnds(20)
      integer icmdfd,cstart
	Common / MiscCVar / FileNm, CmdStr, Cmnds, WhoIAm ,icmdfd, cstart


C# Command type.
	Character*1 CType(60)
C# Command level.
	Character*80 Level(60)
C# Command name.
	Character*80 IMenu(60)
	Common / MenuCmds / CType, IMenu, Level

C# Version number.
	Real RVrsn(60)
	Common / VerNums / RVrsn


C# Nested levels within program.
	Integer LvlNum
C# Misc sorting area.
	Integer SrtAra(10)
	Common / Nest / LvlNum, SrtAra
C# System call stuff
        Character*6 systuff(8)
        common/ sys /systuff
C# Color codes
        Common /color/ icolor,kcolor(40)
