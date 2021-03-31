 
      SUBROUTINE pendv(xx,yy,np,idev)
C
C ***********************************************************
C
C             S U B R O U T I N E    P L O T D V
C FUNCTION:
C             APPLIES ROTATION TO PLOT
C PARAMETERS:
C             (X,Y)     IN  - THE POINT TO MOVE OR DRAW TO.
C                             D2
C             NP        IN  - THE PEN NUMBER, 2 FOR DOWN, 3 FOR UP.
C                             D4
C             IDEV      IN  - DUMMY VARIABLE, WHICH MAY SOMEDAY BE
C                             USED TO MAKE THIS A DEVICE INDEPENDENT
C                             DRIVER
C ROUTINES
C CALLED:     PLOT.
C
C REVISED FROM MOSS:PLOTDV BY MIKE FIEBACH, TGS, FEB 1985 (ALLOW ROTATION)
C
C ***************************************************************
C
      COMMON /rotate/ thetad , turn , scal
      COMMON /cparam/ icpar(128) , cpar(64) , ipar(20,20)
      LOGICAL turn
C
C  IF ROTATION IS TO BE APPLIED, TRANSLATE TO LOWER LEFT CORNER OF
C  MBR, ROTATE AND TRANSLATE BACK TO ORIGINAL OFFSET
C
      xnew = xx
      ynew = yy
      IF ( turn ) THEN
         thetar = thetad*3.1416/180.
         xoff = (cpar(11))/2.
         yoff = (cpar(12))/2.
         xold = xx
         yold = yy
         xnew = (xold-xoff)*cos(thetar) - (yold-yoff)*sin(thetar) + xoff
         ynew = (xold-xoff)*sin(thetar) + (yold-yoff)*cos(thetar) + yoff
      ENDIF
C
C  CALL DEVICE ROUTINE CALLED 'PLOT'
C
      IF ( idev.EQ.1 ) THEN
C       CALL tkPLOT(XNEW,YNEW,NP)
      ELSEIF ( idev.EQ.2 ) THEN
         CALL hpplot(xnew,ynew,np)
      ENDIF
      RETURN
      END
