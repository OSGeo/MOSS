C******************************************************************************
C
C     NAME:  SUBROUTINE BLOCK
C
C     FUNCTION:  THIS ROUTINE CHECKS FOR DATA POINTS THAT ARE BLOCKED
C                FROM THE TARGET CELL BECAUSE EITHER THE POINT OR TARGET
C                CELL IS ENCLOSED BY FAULTS, OR IF EITHER ANGLE FORMED
C                BY THE TARGET CELL AND FAULT OR BY THE FAULT AND DATA
C                POINT IS LESS THAN 60 DEGREES.
C
C     PARAMETERS:  NAME     USE     TYPE     DIMENSION     RANGE
C                  -------------------------------------------------
C                  TXCELL    I      REAL
C                  THE X COORDINATE OF THE CENTER OF THE CELL
C                  -------------------------------------------------
C                  TYCELL    I      REAL
C                  THE Y COORDINATE OF THE CENTER OF THE CELL
C                  -------------------------------------------------
C                  IER       O      INTEGER
C                  ERROR INDICATOR (1 => NORMAL)
C                  -------------------------------------------------
C
C     ROUTINES REQUIRED:  GETFAST, RDBLK, CORDXY, SINTER, ANGLE
C
C     CALLING ROUTINES:  FAULT
C
C
      SUBROUTINE BLOCK ( TXCELL, TYCELL, IER )
C
$Include:'xypnts.inc'
       COMMON /FLTDAT/ FMAPID,ICHF,ISTAR,ISTOP,NTOT,IFDAT(2,64)
       COMMON /RELTN/  JELTN(50,4), NPOINT(64), NFLAG, NPTS
       COMMON /GWINDO/ BAND(3,50)
       COMMON /WORK/   IBUFF(128), MAP(25), ICOM(10), JCOM(10)
C
       INTEGER     FMAPID,IWIND(8),IBLCK(4)
       REAL        XYV(2),XY1(2),XY2(2),WIND(4),XI(2),YI(2),XL(2),YL(2)
       EQUIVALENCE (IWIND(1),WIND(1))
C
       PARAMETER   (TOL = 0.0)
       PARAMETER   (SIXTY = 1.0471976)
      IER = 0
C
C.....FIND THE POSITION OF THE TARGET CELL TO EACH FAULT AND ALSO
C.....THE POSITION OF THE CANDIDATE DATA POINTS TO EACH FAULT.
C
      DO 500 J = 1,NPTS
        IF (JELTN(J,2) .EQ. 1 .OR. JELTN(J,2) .EQ. 2)  GO TO 500
          IFLT = JELTN(J,3)
          DO 50 I = 1, NFLAG
            IF ( NPOINT(I).EQ.IFLT ) GO TO 75
   50     CONTINUE
C.........SHOULD NEVER ( NEVER ) GET HERE !
          GO TO 950
C
   75     INDY = I
          IF (IFDAT(1,INDY) .NE. 1 .OR. IFDAT(2,INDY) .NE. 1) GO TO 100
            JELTN(J,2) = 2
            GO TO 500
C
  100     DO 150 K = 1,4
            IBLCK(K) = 0
  150     CONTINUE
C
          IPL = 1
          CALL GETFAST(NPOINT(INDY),IWIND,INDX,ISUBP,IEX1,IEX2,IER)
          IF (IER .NE. 0) GO TO 900
          CALL RDBLK(ICHF,INDX,IBUFF,128,IER)
          IF (IER .NE. 0) GO TO 910
          CALL CORDXY(IBUFF,NPNTS,INDX,WIND(1),WIND(3),ICHF)
C
          call spoints('X ',x,x1,1,1)
          call spoints('Y ',y,y1,1,1)
          call spoints('X ',x,xn,npnts,1)
          call spoints('Y ',y,yn,npnts,1)
c          X1 = X(1)
c          Y1 = Y(1)
c          XN = X(NPNTS)
c          YN = Y(NPNTS)
C
C.........FIND THE ANGLE BETWEEN THE TARGET CELL AND FAULT AND THE ANGLE
C.........BETWEEN THE FAULT AND DATA POINT.  IF EITHER OF THESE IS LESS
C.........THAN 60 DEGREES, THE DATA POINT IS "BLOCKED" AND SHOULD NOT BE
C.........CONSIDERED IN THE INTERPOLATION PROCESS.  FIRST WE MUST FIND WHICH
C.........END OF THE FAULT LINE IS CLOSEST.
C
          DX1 = TXCELL - X1
          DY1 = TYCELL - Y1
          DIST1 = (DX1 * DX1) + (DY1 * DY1)
          DX1 = X1 - BAND(1,J)
          DY1 = Y1 - BAND(2,J)
          DIST1 = DIST1 + (DX1 * DX1) + (DY1 * DY1)
          DX1 = TXCELL - XN
          DY1 = TYCELL - YN
          DIST2 = (DX1 * DX1) + (DY1 * DY1)
          DX1 = XN - BAND(1,J)
          DY1 = YN - BAND(2,J)
          DIST2 = DIST2 + (DX1 * DX1) + (DY1 * DY1)
          IF (DIST1 .GT. DIST2) GO TO 200
            XYV(1) = X1
            XYV(2) = Y1
            call spoints('X ',x,xy2(1),2,1)
            call spoints('Y ',y,xy2(2),2,1)
c            XY2(1) = X(2)
c            XY2(2) = Y(2)
            GO TO 250
  200     XYV(1) = XN
          XYV(2) = YN
          call spoints('X ',x,xy2(1),npnts-1,1)
          call spoints('Y ',y,xy2(2),npnts-1,1)
c          XY2(1) = X(NPNTS-1)
c          XY2(2) = Y(NPNTS-1)
C
  250     XY1(1) = TXCELL
          XY1(2) = TYCELL
          CALL ANGLE(XYV,XY1,XY2,ANGL)
          IF (ABS(ANGL) .LT. SIXTY)  JELTN(J,2) = 2
          IF (ABS(ANGL) .LT. SIXTY)  GO TO 500
            XY1(1) = BAND(1,J)
            XY1(2) = BAND(2,J)
            CALL ANGLE(XYV,XY1,XY2,ANGL)
            IF (ABS(ANGL) .LT. SIXTY)  JELTN(J,2) = 2
            IF (ABS(ANGL) .LT. SIXTY)  GO TO 500
C
C...........IF NOT BLOCKED BY THE ANGLES, CHECK TO SEE IF IT IS CLOSED OFF
C
            NBLCK = 0
            DO 450 IND = 1,4
              IF (IND .GT. 1) GO TO 300
                IF (IFDAT(1,INDY) .EQ. 1)  GO TO 450
                  XMIN = MIN(TXCELL,X1)
                  XMAX = MAX(TXCELL,X1)
                  YMIN = MIN(TYCELL,Y1)
                  YMAX = MAX(TYCELL,Y1)
                  XVAL1 = TXCELL
                  XVAL2 = X1
                  YVAL1 = TYCELL
                  YVAL2 = Y1
                  GO TO 375
C
  300         IF (IND .GT. 2) GO TO 325
                IF (IFDAT(1,INDY) .EQ. 1)  GO TO 450
                  XMIN = MIN(BAND(1,J),X1)
                  XMAX = MAX(BAND(1,J),X1)
                  YMIN = MIN(BAND(2,J),Y1)
                  YMAX = MAX(BAND(2,J),Y1)
                  XVAL1 = BAND(1,J)
                  XVAL2 = X1
                  YVAL1 = BAND(2,J)
                  YVAL2 = Y1
                  GO TO 375
C
  325         IF (IND .GT. 3) GO TO 350
                IF (IFDAT(2,INDY) .EQ. 1)  GO TO 450
                  XMIN = MIN(BAND(1,J),XN)
                  XMAX = MAX(BAND(1,J),XN)
                  YMIN = MIN(BAND(2,J),YN)
                  YMAX = MAX(BAND(2,J),YN)
                  XVAL1 = BAND(1,J)
                  XVAL2 = XN
                  YVAL1 = BAND(2,J)
                  YVAL2 = YN
                  GO TO 375
C
  350         IF (IFDAT(2,INDY) .EQ. 1)  GO TO 450
                XMIN = MIN(TXCELL,XN)
                XMAX = MAX(TXCELL,XN)
                YMIN = MIN(TYCELL,YN)
                YMAX = MAX(TYCELL,YN)
                XVAL1 = TXCELL
                XVAL2 = XN
                YVAL1 = TYCELL
                YVAL2 = YN
C
  375         DO 425 I = 1,NFLAG
                IF (I .EQ. INDY) GO TO 425
                IPL = IPL + 1
                CALL GETFAST(NPOINT(I),IWIND,INDX,ISUBP,IEX1,IEX2,IER)
                IF (IER .NE. 0) GO TO 900
                IF ( INVWND(WIND(1),WIND(2),WIND(3),WIND(4),
     +                      XMIN,   XMAX,   YMIN,   YMAX).NE.1 )
     +                      GO TO 425
                  CALL RDBLK(ICHF,INDX,IBUFF,128,IER)
                  IF (IER .NE. 0) GO TO 910
                    CALL CORDXY(IBUFF,NPNTS,INDX,WIND(1),WIND(3),ICHF)
                    call spoints('X ',x,xk1,1,1)
                    call spoints('Y ',y,yk1,1,1)
c                    XK1 = X(1)
c                    YK1 = Y(1)
C
C...................FOR EACH LINE SEGMENT WITHIN THE LINE FEATURE,
C...................CREATE A MBR.  SEE IF IT INTERSECTS THE MBR
C...................OF THE TARGET CELL CENTER AND THE DATA POINT.
C
                    DO 400 K = 2,NPNTS
                      XK  = XK1
                      call spoints('X ',x,xk1,k,1)
c                      XK1 = X(K)
                      YK  = YK1
                       call spoints('Y ',y,yk1,k,1)
c                      YK1 = Y(K)
                      FXMIN = MIN(XK,XK1)
                      FXMAX = MAX(XK,XK1)
                      FYMIN = MIN(YK,YK1)
                      FYMAX = MAX(YK,YK1)
                      IF ( INVWND(XMIN, XMAX, YMIN, YMAX,
     +                            FXMIN,FXMAX,FYMIN,FYMAX).NE.1 )
     +                            GO TO 400
C
C.......................THE MBRs INTERSECT.  SEE IF THE LINE SEGMENTS
C.......................WITHIN THE MBRs INTERSECT.
C
                        XI(1) = XK
                        XI(2) = XK1
                        YI(1) = YK
                        YI(2) = YK1
                        XL(1) = XVAL1
                        XL(2) = XVAL2
                        YL(1) = YVAL1
                        YL(2) = YVAL2
                        CALL SINTER(XI,YI,XL,YL,XTMP,YTMP,IFLAG,TOL)
                        IF (IFLAG .EQ. 0) GO TO 400
                          IBLCK(IND) = 1
                          NBLCK = NBLCK + 1
                          GO TO 450
C
  400               CONTINUE
  425         CONTINUE
  450       CONTINUE
C
C...........NOW CHECK TO SEE WHAT ENDS ARE BLOCKED.
            IF (IFDAT(1,INDY) .EQ. 1)  NBLCK = NBLCK + 2
            IF (IFDAT(1,INDY) .EQ. 1)  IBLCK(1) = 1
            IF (IFDAT(1,INDY) .EQ. 1)  IBLCK(2) = 1
            IF (IFDAT(2,INDY) .EQ. 1)  NBLCK = NBLCK + 2
            IF (IFDAT(2,INDY) .EQ. 1)  IBLCK(3) = 1
            IF (IFDAT(2,INDY) .EQ. 1)  IBLCK(4) = 1
C
            IF (NBLCK .EQ. 1)  GO TO 500
              IF (NBLCK .GE. 3)  JELTN(J,2) = 2
              IF (IBLCK(1) .EQ. 1 .AND. IBLCK(3) .EQ. 1)  JELTN(J,2) = 2
              IF (IBLCK(2) .EQ. 1 .AND. IBLCK(4) .EQ. 1)  JELTN(J,2) = 2
C
  500 CONTINUE
C
C.....NORMAL RETURN
      GO TO 990
C
900   WRITE(*,901) IER
  901 FORMAT(' *BLOCK* ERROR FROM GETFAST - ERROR ',I6)
      GO TO 990
C
  910 WRITE(*,911) IER, IPL, J, I
  911 FORMAT(' *BLOCK* ERROR FROM RDBLK - ERROR ',4I6)
      GO TO 990
C
  950 IER = -5
      WRITE(*,951) JELTN(J,3), (NPOINT(I),I=1,NFLAG)
  951 FORMAT(' *BLOCK* JELTN(J,3) & NPOINT() = ',I3,(/10X,10I5))
C
  990 RETURN
      END
