      SUBROUTINE poise(ixp,iyp,x1,y1,x2,y2,dsq)
C
C *****************************************************************************
C
C             S U B R O U T I N E     P O I S E
C FUNCTION:
C          COMPUTES THE SQUARE OF THE DISTANCE FROM A POINT TO A LINE SEGMENT.
C          P IS THE POINT AND XY CONTAINS THE END-POINTS OF THE LINE SEGMENT.
C          RETURNS THE SQUARED DISTANCE IN DSQ.
C PARAMETERS:
C             (IXP,IYP)    IN  - POINT P.
C             ((X1,Y1),(X2,Y2)) IN - THE END POINTS OF THE LINE SEGMENTS.
C             DSQ       OUT - THE SQUARED DISTANCE FROM THE POINT P
C                             TO THE LINE SEGMENT.
C ROUTINES
C CALLED:     NONE.
C
C *****************************************************************************
C
      REAL x1 , y1 , x2 , y2 , ixp , iyp
      DOUBLE PRECISION u , v , xx , yy , m , t , dd , dsqr
      u = dble(ixp-x1)
      v = dble(iyp-y1)
      xx = dble(x2-x1)
      yy = dble(y2-y1)
      dd = xx*xx + yy*yy
      IF ( dd.EQ.0.D0 ) THEN
         dsqr = dmin1(u*u+v*v,(u-xx)**2+(v-yy)**2)
      ELSE
         m = u*xx + v*yy
         IF ( m.LT.0.D0 .OR. m.GT.dd ) THEN
            dsqr = dmin1(u*u+v*v,(u-xx)**2+(v-yy)**2)
         ELSE
            t = u*yy - v*xx
            dsqr = (t/dd)*t
         ENDIF
      ENDIF
      dsq = dsqrt(dsqr)
      RETURN
      END
