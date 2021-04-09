C************************************************************************
C
C     NAME:  SUBROUTINE GRIDI1
C
C     FUNCTION:  INTERPOLATE Z VALUES OF CELLS GIVEN A SET OF DISCRETE POINTS.
C                THE ALGORITHM FINDS THE CLOSEST POINT IN EACH OF FOUR QUADRANTS
C                AND THEN USES A DISTANCE WEIGHTED AVERAGING FUNCTION.
C
C     PARAMETERS:  NAME     USE     TYPE     DIMENSION     RANGE
C                  -------------------------------------------------
C                  XCELL    I      R*4                    +REAL
C                  LOCATION OF CELL OF INTEREST IN X DIRECTION
C                  -------------------------------------------------
C                  YCELL    I      R*4                    +REAL
C                  LOCATION OF CELL OF INTEREST IN Y DIRECTION
C                  -------------------------------------------------
C                  NPTS     I      I*2                    +INTEGER
C                  NUMBER OF ROW,COLUMN AND X,Y,ELEVATION VALUES WITHIN
C                  THE ROVING WINDOW
C                  -------------------------------------------------
C                  ZCELL    O      R*4                    +REAL
C                  COMPUTED VALUE OF CELL AT X,Y LOCATION
C                  -------------------------------------------------
C                  CELWIN   I      R*4         4          +REAL
C                  CELL WINDOW FOR CONTAINMENT DETERNIMATION
C                  -------------------------------------------------
C                  IER      O      I*2                     INTEGER
C                  ERROR INDICATOR 1-NO ERROR
C                  -------------------------------------------------
C
C     FILES:  None
C
C     ROUTINES REQUIRED: None
C
C     CALLING ROUTINES:  GRIDMK
C***********************************************************************
C
      SUBROUTINE gridi1(xcell,ycell,npts,zcell,celwin,ier)
C
      COMMON /gwindo/ band(3,50)
      DIMENSION rada(4) , zz(4) , ixyz(6) , xyz(3) , celwin(4)
      REAL smval
      EQUIVALENCE (ixyz(1),xyz(1))
      DATA smval/.0000001/
C
      sr = 0.
      zcell = 0.
C
      DO i = 1 , 4
         rada(i) = -9999999.
         zz(i) = -9999999.
      ENDDO
C
C......FIND THE CLOSEST POINTS IN THE 4 ROTATED QUADRANTS .....
C
      DO i = 1 , npts
         DO m = 1 , 3
            xyz(m) = band(m,i)
         ENDDO
         dx = xcell - xyz(1)
         dy = ycell - xyz(2)
         dist = dx*dx + dy*dy
C
C........IF POINT NOT CONTAINED, SKIP EXACT MATCH CODE
         IF ( xyz(2).LT.celwin(1) .OR. xyz(2).GT.celwin(2) .OR. xyz(1)
     &        .LT.celwin(3) .OR. xyz(1).GT.celwin(4) ) THEN
C
C
C........DETERMINE WHAT QUADRANT .....
            ang = atan2(dy,dx)/.0174533 + 225.
            IF ( ang.GT.360. ) ang = ang - 360.
            iquad = ang/90. + 1
            IF ( rada(iquad).GE.0.0 ) THEN
               IF ( dist.GE.rada(iquad) ) GOTO 100
            ENDIF
         ELSE
C
C........EXACT MATCH FOUND .....
            zcell = xyz(3)
            nexact = 1
C
C........SCAN REST OF POINTS IN CASE SEVERAL IN SAME CELL
            IF ( (i+1).LE.npts ) THEN
               DO j = i + 1 , npts
                  DO m = 1 , 3
                     xyz(m) = band(m,j)
                  ENDDO
                  IF ( xyz(2).GE.celwin(1) .AND. xyz(2).LE.celwin(2)
     &                  .AND. xyz(1).GE.celwin(3) .AND. xyz(1)
     &                 .LE.celwin(4) ) THEN
C
                     zcell = zcell + xyz(3)
                     nexact = nexact + 1
                  ENDIF
               ENDDO
               IF ( nexact.GT.1 ) zcell = zcell/float(nexact)
               IF ( abs(zcell).LT.smval ) zcell = .000001
            ENDIF
            GOTO 200
         ENDIF
C
C..........1ST POINT IN QUADRANT .....
         rada(iquad) = dist
         zz(iquad) = xyz(3)
C..........FARTHER AWAY
 100  ENDDO
C
C......CHECK OUT QUADRANT .....
      DO iquad = 1 , 4
C........NOTHING IN QUADRANT .....
         IF ( abs(rada(iquad)+9999999.0).GE.smval ) THEN
            sr = sr + 1./rada(iquad)
            zcell = zcell + zz(iquad)/rada(iquad)
         ENDIF
      ENDDO
C
C......COMPUTE THE ANSWER .....
      IF ( abs(sr).GT.0.0 ) zcell = zcell/sr
C
C......NORMAL RETURN .....
C
 200  CONTINUE
      RETURN
      END
