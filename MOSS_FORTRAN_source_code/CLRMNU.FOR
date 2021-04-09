C * Subroutine ClrMnu clears the menu areas when a new "program" is called and
C   sets up the system for that programs menus.
 
      SUBROUTINE clrmnu
 
$Include:'GLOBAL.inc'
 
C#    New program name.
      whoiam = ' '
      DO k = 1 , 8
         IF ( imenu(prgrun)(k:k).EQ.' ' ) GOTO 100
         whoiam(k:k) = imenu(prgrun)(k:k)
      ENDDO
C#    Save return.
 100  CONTINUE
      lvlnum = lvlnum + 1
      IF ( lvlnum.GT.1 ) display = .FALSE.
      level(lvlnum) = whoiam
C#    Reset.
      prgrun = 0
C#    Reset command name, command type, and version number in menu.
      DO i = nulcmd + 1 , ionum
         imenu(i) = ' '
         ctype(i) = ' '
         rvrsn(i) = 0.0
      ENDDO
      ionum = nulcmd
      RETURN
      END
