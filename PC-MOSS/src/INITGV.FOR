C * Subroutine InitGV initializes all Global systemwide (global) variables.
 
      SUBROUTINE initgv
 
$Include:'GLOBAL.inc'
 
 
C * Common area / GlbLog / Error, Exit, Help, SysLvl
 
C# No error exists.
      error = .FALSE.
C# Exit when = .True.
      exit = .FALSE.
C# HELP not requested.
      help = .FALSE.
C# Prompt with full menu.
      display = .TRUE.
 
 
C * Comman area / Terminal / BaudR, ChrSet, Echo
 
C# Default receive speed.
      baudr = 9600
C# Default character set:  0=ASCII; 1=EBCDIC.
      chrset = 0
C# Echo:  0=On/Full duplex; 1=Off/Half duplex.
      echo = 0
 
 
C * Common / MenuSel / AlfCmd, IONum:  set at run time.
C * Common / GTime / CDate, CTime, IDate, ITime:  set at run time.
 
C * Common / GlbIO / SrchIO, UsrIn, UsrOut
 
C# Miscellaneous channel for user files.
      srchio = 20
C# User 'terminal' output device/channel (F77).
C   :  DG (6;10); VAX; DEC 20's; PC DOS, Ryan-MacFarland.
C        UsrOut = 5
C   :  McIntosh.
C	UsrOut = 9
C   :  PC DOS, Microsoft.
      usrout = 0
C# User 'terminal' input device/channel (F77).
C   :  DG (11); VAX; DEC 20's; PC DOS, Ryan-MacFarland.
C        UsrIn = 5
C   :  McIntosh.
C	UsrIn = 9
C   :  PC DOS, Microsoft.
      usrin = 0
 
 
 
 
C * Common / MiscIVar / FrsChr, IEr, NulCmd, NumCmd, RetLbl.
C   Note that NulCmd should be set in subroutine LdCmd to reflect the
C   number of commands loaded at that time.
 
C# Starting position main command.
      frschr = 0
C# Error indicator.
      ier = 0
C# Built in commands.
      nulcmd = 0
C# Program generated commands.
      numcmd = 0
C# Index variable for returns.
      retlbl = 0
C# Starting position for sub-command.
      secchr = 0
 
 
C * Commons / MiscRVar /, / MiscCVar /, / MenuCmds /, and / VerNums /
C   are set at run time (i.e. dependent upon runtime conditions).
C
C * set valid system call strings
C
      systuff(1) = 'QUIT'
      systuff(2) = 'DIR/P'
      systuff(3) = 'COPY'
      systuff(4) = 'PRINT'
      systuff(5) = 'PLOTIT'
      systuff(6) = 'CHKDSK'
      systuff(7) = 'CD'
C
C Display color number
C
      icolor = 1
C
      RETURN
      END
