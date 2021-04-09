C *****************************************************************************
C
C             P R O G R A M     T A B L E
C FUNCTION:
C          THIS ROUTINE RE-STATE MOSS RUNTIME PARAMETERS AND EXECUTE THE
C          AREA, PERIMETER, LENGTH, AND FREQUENCY COMMANDS
C *****************************************************************************
      PROGRAM perimete
C
$Include:'dbchan.inc'
$Include:'mfast.inc'
      COMMON /fls   / iunit , ifile(5)
$Include:'strng.inc'
C
C  FIRST READ IN THE COMMON BLOCKS
C
      CALL initl(ier)
      IF ( ier.NE.0 ) STOP
C
C  OPEN POINTER FILE
C
      CALL openf(icdcfa,idesfa,1,512,2,ier)
      IF ( ier.NE.0 ) THEN
C
C ERROR RETURNS
C
         WRITE (*,*) ' Could not properly open <DESCRIBE> file'
      ELSE
C
C  SET WINDOW
C
         CALL setwnd
C
C  CALL TABLE GENERATION ROUTINE
C
         icard(80) = -4
         CALL tabara(lines,icard(80))
      ENDIF
C
      lines = lines + 2
      CLOSE (icdcfa,IOSTAT=ier)
      END
