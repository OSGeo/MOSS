      SUBROUTINE setwnd
C
C *****************************************************************************
C
C             S U B R O U T I N E     S E T W N D
C FUNCTION:
C             THIS ROUTINE SETS VIRTUAL AND SCREEN WINDOWS.
C PARAMETERS:
C             NONE.
C ROUTINES
C CALLED:     WINDO,SCRSET.
C
C *****************************************************************************
C
$Include:'vwnd.inc'
      IF ( ibw.EQ.1 ) THEN
c        if(xstart+ystart.le..001.or.xlen+ylen.le..001)then
c         write(*,*)'You must SELECT and WINDOW a map first'
c         stop
c        endif
         CALL vwindo(xstart,xlen,ystart,ylen)
         CALL scrset(xstart,xlen,ystart,ylen)
         RETURN
      ELSE
         IF ( xorgin+xalen.LE..001 .OR. yorgin+yalen.LE..001 ) THEN
            WRITE (*,*)
            WRITE (*,*) 'You must WINDOW a map first'
            STOP
         ENDIF
         CALL vwindo(xorgin,xalen,yorgin,yalen)
         CALL scrset(xorgin,xalen,yorgin,yalen)
         RETURN
      ENDIF
      END
