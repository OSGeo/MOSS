      SUBROUTINE calmbr(npts,xmin,xmax,ymin,ymax)
C
C *****************************************************************************
C
C             S U B R O U T I N E     C A L M B R
C FUNCTION:
C             CALCULATE MBR FOR AN ITEM
C PARAMETERS:
C             NPTS      - NUMBER OF MAP POINTS.
C             XMIN,XMAX,YMIN,YMAX   O- THE X AND Y, MINIMUN AND MAXIMUN
C                                      OF THE MAP DATA.
C ROUTINES
C *****************************************************************************
C
$Include:'xypnts.inc'
C  SET VALUES
C
      xmin = 9999999.0
      xmax = 0.0
      ymin = 9999999.0
      ymax = 0.0
C
C  ENTER LOOP
C
      DO i = 1 , npts
         CALL spoints('X ',x,xi,i,1)
         CALL spoints('Y ',y,yi,i,1)
         xmin = amin1(xi,xmin)
         xmax = amax1(xi,xmax)
         ymin = amin1(yi,ymin)
         ymax = amax1(yi,ymax)
      ENDDO
C
      RETURN
      END
