      SUBROUTINE drplus(xpoint,ypoint)
C *********************************************************************
C
C                S U B R O U T I N E    D R P L U S
C
C  Purpose:  This routine draws a plus centered at xpoint,ypoint
C
C  Parameters:
C     Name       Use        Type         Dimension       Description
C     ----       ---        ----         ---------   ------------------
C
C  Files:    Logical Unit       File name or description
C            ------------       ------------------------
C
C  Calls: WHWND, INVWND, MOVEA, MOVREL, DRWREL
C
C **********************************************************************
      COMMON /mdevice/ idev
$Include:'IO.inc'
C
C
C  FIRST CHECK TO ENSURE THAN IT IS IN THE WINDOW
C
      CALL whwnd(x1,y1,x2,y2)
      IF ( invwnd(xpoint,xpoint,ypoint,ypoint,x1,x2,y1,y2).NE.0 ) THEN
C
C  DRAW + CENTERED AT XPOINT, YPOINT
C
         CALL plotdv(xpoint,ypoint,3,idev)
         CALL plotdv(xpoint-3,ypoint,3,idev)
         CALL plotdv(xpoint+6,ypoint,2,idev)
         CALL plotdv(xpoint-3,ypoint,3,idev)
         CALL plotdv(xpoint,ypoint-3,3,idev)
         CALL plotdv(xpoint,ypoint+6,2,idev)
      ENDIF
C
      RETURN
      END
