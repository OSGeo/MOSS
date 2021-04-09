C******************************************************************
C
C       PROGRAM:        S U B R O U T I N E     D R A W B
C
C       FUNCTION:       DRAWS A BORDER AROUND THE MINIMUM BOUNDING
C                       RECTANGLE
C
C       PARAMETERS:     X=X DIRECTION DISTANCE IN INCHES FROM XMIN
C                               & XMAX SIDES
C                       Y=Y DIRECTION DISTANCE IN INCHES FROM YMIN
C                               & YMAX SIDES
C
C                       YOFF=Y OFFSET FROM BOTTOM OF BORDER
C
C
C       ROUTINES REQUIRED:      hpplot
C                               COMMON DIMEN
C
C******************************************************************
      SUBROUTINE drawb(x,y,yoff)
      COMMON /dimen / x1 , y1 , x2 , y2 , xln , yln , scale , fact , 
     &                x0 , y0
C
C
      xl = x - 2.0*fact
      yb = 0.0 - yoff
      xr = x + xln + 1.0*fact
      yt = y + yln + 1.0*fact
      CALL hpplot(xl,yb,3)
      CALL hpplot(xr,yb,2)
      CALL hpplot(xr,yt,2)
      CALL hpplot(xl,yt,2)
      CALL hpplot(xl,yb,2)
C
      RETURN
      END
