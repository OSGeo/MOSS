      LOGICAL FUNCTION fpoint(x,y,xp,yp,atol)
      REAL atol , x , y , xp , yp
      DOUBLE PRECISION dist , xdist , ydist
C
C ************************************************************
C
C       L O G I C A L     F U N C T I O N    F P O I N T
C
C FUNCTION:   DETERMINE IF A DATA POINT IS WITHIN THE
C             SEARCH TOLERENCE OF A SPECIFIED SEARCH POINT
C PARAMETERS:
C             XP,YP  -  ARRAY CONTAINING TEST DATA COORDINATE
C             X,Y   -  SPECIFIED SEARCH COORDINATE
C             ATOL  -  SEARCH RADIUS
C ROUTINES
C CALLED:     NONE.
C ************************************************************
C
      fpoint = .FALSE.
C
C  CALCULATE DISTANCE BETWEEN POINT
C
      xdist = dble(x-xp)
      ydist = dble(y-yp)
      dist = xdist**2 + ydist**2
C     DIST=SQRT(DIST)
      IF ( dist.GT.atol**2 ) RETURN
C     IF(DIST.GT.ATOL) RETURN
      fpoint = .TRUE.
C
      RETURN
      END
