C***********************************************************************
C
C     NAME:  SUBROUTINE IFLINE
C
C     FUNCTION:  THIS ROUTINE, GIVEN A (X,Y) DATA POINT AND THE NUMBER
C                OF POINTS IN A LINE FEATURE, RETURNS 2 INDICES INTO THE
C                (X,Y) DISK ARRAYS.  THE FIRST INDEX, "IND1", IS THE
C                INDEX OF THE CLOSEST FEATURE POINT TO THE INPUT DATA
C                POINT.  THE SECOND INDEX, "IND2", IS THE INDEX OF THE
C                POINT ON EITHER SIDE OF "IND1".  THE (X,Y) POINTS
C                REPRESENTED BY "IND1" AND "IND2" FORM A SEGMENT OF THE
C                LINE FEATURE THAT LIES CLOSEST TO THE INPUT DATA POINT.
C
C     PARAMETERS:  NAME     USE     TYPE     DIMENSION     RANGE
C                  -------------------------------------------------
C                  XT        I      REAL                   +REAL
C                  THE X COORDINATE OF THE DATA POINT
C                  -------------------------------------------------
C                  YT        I      REAL                   +REAL
C                  THE Y COORDINATE OF THE DATA POINT
C                  -------------------------------------------------
C                  NPTS      I     INTEGER                +INTEGER
C                  NUMBER OF POINTS IN THE LINE FEATURE
C                  -------------------------------------------------
C                  IND1      O     INTEGER                +INTEGER
C                  INDEX INTO THE (X,Y) DISK ARRAY
C                  -------------------------------------------------
C                  IND2      O     INTEGER                +INTEGER
C                  INDEX INTO THE (X,Y) DISK ARRAY
C                  -------------------------------------------------
C                  XVAL      O     REAL
C                  THE X VALUE OF THE POINT ON THE LINE
C                  -------------------------------------------------
C                  YVAL      O      REAL
C                  THE Y VALUE OF THE POINT ON THE LINE
C                  -------------------------------------------------
C
C     ROUTINES REQUIRED:  POISE.OVER
C
C     CALLING ROUTINES:   ISIDE
C
C***********************************************************************
C
      SUBROUTINE ifline(xt,yt,npts,ind1,ind2,xval,yval)
C
$Include:'xypnts.inc'
      dist = 9999999
      ind1 = 1
      CALL spoints('X ',x,x2,1,1)
      CALL spoints('Y ',y,y2,1,1)
c       X2 = X(1)
c       Y2 = Y(1)
      DO i = 2 , npts
         x1 = x2
         y1 = y2
         CALL spoints('X ',x,x2,i,1)
         CALL spoints('Y ',y,y2,i,1)
c         X2 = X(I)
c         Y2 = Y(I)
         CALL poise(xt,yt,x1,x2,y1,y2,xv,yv,dsq)
         tdist = sqrt(dsq)
         IF ( tdist.LE.dist ) THEN
            dist = tdist
            ind1 = i - 1
            ind2 = i
            xval = xv
            yval = yv
         ENDIF
C
      ENDDO
C
      IF ( ind1.NE.1 ) THEN
         IF ( xv.LT.x(ind1) .AND. xv.GT.x(ind1-1) ) ind2 = ind1 - 1
      ENDIF
C
      RETURN
      END
