C***********************************************************************
C  SUBROUTINE ANGLE
C
C     FUNCTION : TO FIND THE ANGLE BETWEEN 2 LINES WITH
C                VERTEX(VERT(1),VERT(2))
C
C     PARAMETERS:  NAME     USE     TYPE     DIMENSION     RANGE
C                  -------------------------------------------------
C                  VERT      I    REAL ARRAY     2
C                  THE VERTEX OF THE ANGLE FORMED BY THE 2 LINES
C                  -------------------------------------------------
C                  PT1       I    REAL ARRAY     2
C                  THE END POINT OF THE FIRST LINE
C                  -------------------------------------------------
C                  PT2       I    REAL ARRAY     2
C                  THE END POINT OF THE SECOND LINE
C                  -------------------------------------------------
C                  ANGL      O      REAL
C                  THE ANGLE IN RADIANS
C                  -------------------------------------------------
C
C     ROUTINES REQUIRED:  AZIMUT
C
C     CALLING ROUTINES:  BLOCK
C     CITATIONS:  BASED ON ALGORITHM FOUND IN "CARTOGRAPHIC DRAWING WITH
C                 COMPUTER" BY P. YOELI, VOLUME 8, COMPUTER APPLICATIONS
C                 SPECIAL ISSUE, 1982
C
C***********************************************************************
C
      SUBROUTINE angle(vert,pt1,pt2,angl)
C
C
      REAL vert(2) , pt1(2) , pt2(2)
C
      PARAMETER (dpi=3.14159265)
      PARAMETER (d2pi=2.0*dpi)
C
      CALL azimut(vert(1),vert(2),pt1(1),pt1(2),az1)
      CALL azimut(vert(1),vert(2),pt2(1),pt2(2),az2)
C
      angl = abs(az2-az1)
C
      IF ( angl.GT.dpi ) angl = angl - dpi
C
      RETURN
      END
