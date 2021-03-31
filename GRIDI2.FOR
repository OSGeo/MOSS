C******************************************************************************
C
C     NAME:  SUBROUTINE GRIDI2
C
C     FUNCTION:  GIVEN A SET OF DISCRETE POINTS (MINIMUM OF 2, MAXIMUM
C                OF 12 (KSER)), A DISTANCE WEIGHTED AVERAGING FUNCTION
C                IS USED TO INTERPOLATE Z VALUES FOR A TARGET CELL.
C
C     PARAMETERS:  NAME     USE     TYPE     DIMENSION     RANGE
C                  -------------------------------------------------
C                  XCELL    I      R*4                    +REAL
C                  LOCATION OF CELL OF INTEREST IN X DIRECTION
C                  -------------------------------------------------
C                  YCELL    I      R*4                    +REAL
C                  LOCATION OF CELL OF INTEREST IN Y DIRECTION
C                  -------------------------------------------------
C                  KSER     I      I*2                    +INTEGER
C                  MAXIMUM NUMBER OF POINTS TO USE 2-12
C                  -------------------------------------------------
C                  ZCELL    O      R*4                    +REAL
C                  COMPUTED VALUE OF CELL AT X,Y LOCATION
C                  -------------------------------------------------
C                  CELWIN   I      R*4          4         +REAL
C                  TARGET CELL WINDOW LIMITS
C                  -------------------------------------------------
C                  IER      O      I*2                     INTEGER
C                  ERROR INDICATOR 1-NO ERROR
C                  -------------------------------------------------
C
C     FILES:  None
C
C     ROUTINES REQUIRED:  DISTNCE
C
C     CALLING ROUTINES:  GRIDMK
C
C******************************************************************************
C
      SUBROUTINE gridi2(xcell,ycell,kser,zcell,celwin,ier)
C
      COMMON /reltn / jeltn(50,4) , npoint(64) , nflag , npts
      COMMON /gwindo/ band(3,50)
C
      REAL rada(8) , zz(8)
      REAL xyz(3) , celwin(4) , smval
      INTEGER badpts
      DATA smval/.0000001/
C
      sr = 0.0
      zcell = 0.0
      dist = 0.0
      badpts = 0
C
      DO i = 1 , kser
         zz(i) = -9999999.
         rada(i) = -9999999.
      ENDDO
C
C..FIND KSER CLOSEST POINTS
C
      DO i = 1 , npts
         DO m = 1 , 3
            xyz(m) = band(m,i)
         ENDDO
C
C......CHECK IF FAULT LINES EXIST.  IF THE POINT IS BLOCKED, SKIP IT.
C......OTHERWISE, CALCULATE THE DISTANCE FROM THE TARGET CELL TO THE
C......DATA POINT.
C
         IF ( nflag.NE.0 ) THEN
C
C....KEEP TRACK OF THE NUMBER OF BAD POINTS ENCOUNTERED.
C
            IF ( jeltn(i,2).EQ.2 ) badpts = badpts + 1
            IF ( jeltn(i,2).EQ.2 ) GOTO 100
C
            IF ( jeltn(i,2).NE.1 ) THEN
C
               dist = distnce(i,xcell,ycell,ier)
               IF ( ier.EQ.0 ) GOTO 50
C
C....ERROR RETURN...
C
               WRITE (*,99001) ier
99001          FORMAT (' *GRIDI2* ERROR FROM DISTNCE - ERROR ',I4)
               GOTO 200
            ENDIF
         ENDIF
C
         dx = xcell - xyz(1)
         dy = ycell - xyz(2)
         dist = dx*dx + dy*dy
C
C..IF POINT NOT CONTAINED, SKIP EXACT MATCH CODE
C
 50      CONTINUE
         IF ( xyz(2).LT.celwin(1) .OR. xyz(2).GT.celwin(2) .OR. xyz(1)
     &        .LT.celwin(3) .OR. xyz(1).GT.celwin(4) ) THEN
C
C..NOT EXACT MATCH.  DETERMINE IF THIS IS A VALID SAMPLE POINT
C
            max = i - badpts
C
C...DECREMENT 'I' BY THE NUMBER OF BLOCKED DATA POINTS BEFORE COMPARISON
C
            IF ( max.GT.kser ) THEN
               zmax = -9999999.
C
               DO j = 1 , kser
                  IF ( zmax.LE.rada(j) ) THEN
                     max = j
                     zmax = rada(j)
                  ENDIF
               ENDDO
C
               IF ( dist.GT.zmax ) GOTO 100
            ENDIF
         ELSE
C
C..EXACT MATCH FOUND
C
            zcell = xyz(3)
            nexact = 1
C
C..SCAN REST OF POINTS IN CASE SEVERAL IN SAME CELL
C
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
         zz(max) = xyz(3)
         rada(max) = dist
 100  ENDDO
C
C..CHECK OUT QUADRANT
C
      DO iquad = 1 , kser
C
C..NOTHING IN QUADRANT
C
         IF ( abs(rada(iquad)+9999999.0).GE.smval ) THEN
            sr = sr + 1./rada(iquad)
            zcell = zcell + zz(iquad)/rada(iquad)
         ENDIF
      ENDDO
C
C......COMPUTE THE ANSWER .....
C
C
C..NORMAL RETURN
      IF ( abs(sr).GT.smval ) zcell = zcell/sr
C
 200  CONTINUE
      RETURN
      END
