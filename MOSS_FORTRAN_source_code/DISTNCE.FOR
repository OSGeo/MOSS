C***********************************************************************
C
C     NAME:  REAL FUNCTION DISTNCE
C
C     FUNCTION:  THIS ROUTINE CALCULATES THE DISTANCE FROM A TARGET
C                CELL TO A POINT AROUND A FAULT LINE.  THE DISTANCE IS
C                CALCULATED FROM THE TARGET CELL TO THE END OF THE LINE,
C                AND FROM THE END OF THE LINE TO THE DATA POINT.
C
C     PARAMETERS:  NAME     USE     TYPE     DIMENSION     RANGE
C                  -------------------------------------------------
C                  IND       I     INTEGER
C                  THE INDEX OF THE CANDIDATE POINT (FOR GWINDO)
C                  -------------------------------------------------
C                  XCELL     I      REAL
C                  THE X COORDINATE OF THE TARGET CELL CENTER
C                  -------------------------------------------------
C                  YCELL     I      REAL
C                  THE Y COORDINATE FO THE TARGET CELL CENTER
C                  -------------------------------------------------
C                  IER       O     INTEGER
C                  ERROR INDICATOR (NORMAL = 0)
C                  -------------------------------------------------
C
C***********************************************************************
C
      REAL FUNCTION distnce(ind,xcell,ycell,ier)
C
$INCLUDE:  'GWIND.INC'
$INCLUDE:  'XYPNTS.INC'
      COMMON /work  / ibuff(128) , map(25) , icom(10) , jcom(10)
      COMMON /fltdat/ fmapid , ichf , istar , notno(2) , ifdat(2,64)
      COMMON /reltn / jeltn(50,4) , npoint(64) , nflag , npts
C
      INTEGER iwind(8) , fmapid
      REAL wind(4)
      EQUIVALENCE (iwind(1),wind(1))
C
      iflt = jeltn(ind,3)
      CALL getfast(iflt,iwind,indx,isubp,iex1,iex2,ier)
      IF ( ier.NE.0 ) THEN
C
         WRITE (*,99001) ier
99001    FORMAT (' *DISTNCE* ERROR FROM GETFAST - ERROR ',I6)
      ELSE
         CALL rdblk(ichf,indx,ibuff,1,ier)
         IF ( ier.NE.0 ) THEN
C
            WRITE (*,99002) ier
99002       FORMAT (' *DISTNCE* ERROR FROM rdblk - ERROR ',I6)
         ELSE
            CALL cordxy(ibuff,npnts,indx,wind(1),wind(3),ichf)
C
C....FIND DISTANCES FROM TARGET CELL TO END OF FAULT
C
            CALL spoints('X ',x,x1,1,1)
            CALL spoints('Y ',y,y1,1,1)
            CALL spoints('X ',x,xn,npnts,1)
            CALL spoints('Y ',y,yn,npnts,1)
c      X1 = X(1)
c      XN = X(NPNTS)
c      Y1 = Y(1)
c      YN = Y(NPNTS)
            dx1 = xcell - x1
            dy1 = ycell - y1
            dx2 = xcell - xn
            dy2 = ycell - yn
C
C....FIND DISTANCES FROM END OF FAULT TO DATA POINT
C
            dp1 = x1 - band(1,ind)
            dq1 = y1 - band(2,ind)
            dp2 = xn - band(1,ind)
            dq2 = yn - band(2,ind)
C
            dist1 = (dx1*dx1+dy1*dy1) + (dp1*dp1+dq1*dq1)
            dist2 = (dx2*dx2+dy2*dy2) + (dp2*dp2+dq2*dq2)
C
            distnce = dist1
C
C.....NORMAL RETURN
            IF ( dist1.GT.dist2 ) distnce = dist2
         ENDIF
      ENDIF
C
      RETURN
      END
