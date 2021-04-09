C***********************************************************************
C
C     NAME:  SUBROUTINE AZIMUT
C
C     FUNCTION:  THIS ROUTINE COMPUTES THE AZIMUTH FROM A POINT (XA,YA)
C                TO A POINT (XB,YB).  AZLINE RETURNS THE VALUE OF THE
C                COMPUTER AZIMUTH EXPRESSED IN RADIANS.
C
C     PARAMETERS:  NAME     USE     TYPE     DIMENSION     RANGE
C                  -------------------------------------------------
C                  XA        I      REAL
C                  THE X COORDINATE OF THE FIRST POINT
C                  -------------------------------------------------
C                  YA        I      REAL
C                  THE Y COORDINATE OF THE FIRST POINT
C                  -------------------------------------------------
C                  XB        I      REAL
C                  THE X COORDINATE OF THE SECOND POINT
C                  -------------------------------------------------
C                  YB        I      REAL
C                  THE B COORDINATE OF THE SECOND POINT
C                  -------------------------------------------------
C                  AZLINE    O      REAL
C                  VALUE OF THE AZIMUT IN RADIANS
C                  -------------------------------------------------
C
C     ROUTINES REQUIRED:  NONE
C
C     CALLING ROUTINES:  RECTFY
C***********************************************************************
C
      SUBROUTINE azimut(xa,ya,xb,yb,azline)
C
      PARAMETER dpi = 3.1415926
      PARAMETER d2pi = 2*dpi
C
C....CHECK TO SEE IF THE LINE IS VERTICAL
C
      IF ( abs(xa-xb).GT.0.0001 ) THEN
C
         delx = abs(xb-xa)
         dely = abs(yb-ya)
         IF ( dely.EQ.0.0 ) dely = 0.0000001
         tang = delx/dely
C
         IF ( (xb.GE.xa) .AND. (yb.GE.ya) ) THEN
C
            azline = atan(tang)
         ELSEIF ( (xb.GE.xa) .AND. (yb.LE.ya) ) THEN
C
            azline = dpi - atan(tang)
         ELSEIF ( (xb.LE.xa) .AND. (yb.LE.ya) ) THEN
C
            azline = dpi + atan(tang)
         ELSE
C
            azline = d2pi - atan(tang)
         ENDIF
      ELSE
         IF ( yb.GT.ya ) azline = 0.
         IF ( yb.LT.ya ) azline = dpi
      ENDIF
C
      RETURN
      END
