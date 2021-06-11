C * Subroutine SetCmd gives the basic commands available to every program.
 
      SUBROUTINE lodmnu
 
$Include:'GLOBAL.inc'
 
 
C * The menu is given in it's full form unless SysLevel = .True., in which
C   case, only a simple prompt ('*') is given.
 
C# Assign program file name.
      filenm = ' '
      DO l = 1 , 8
         IF ( whoiam(l:l).NE.' ' ) THEN
            filenm(l:l) = whoiam(l:l)
            cstart = l
         ENDIF
      ENDDO
C# Load user commands to memory.
      CALL ldcmd
      RETURN
      END
