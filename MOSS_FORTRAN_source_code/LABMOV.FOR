C
C *********************************************************************
C
C                S U B R O U T I N E   L A B M O V
C
C  Purpose:   MOVE a label location for a feature that falls in the
C             display window so that the feature label also falls
C             in the display window
C
C  Parameters:
C     Name       Use        Type         Dimension       Description
C     ----       ---        ----         ---------   ------------------
C
C     MBR        IN         REAL             4       FEATURE MBR
C     NVERT      IN         INT           2 BYTES    NUMBER OF VERTICES
C     IDTYP      IN         INT           2 BYTES    DATA TYPE 1-POINT,2-LINE
C                                                    3 - POLYGON
C     XCENT      IN/OUT     REAL          4 BYTES    FUDGE LABEL LOC
C     YCENT      IN/OUT     REAL          4 BYTES    FUDGE LABEL LOC
C     IER        OUT        INT           2/4 BYTES  ERROR RETURN
C
C  Files:    Logical Unit       File name or description
C            ------------       ------------------------
C            ICHX               X DISK ARRY
C            ICHY               Y DISK ARRY OF COORDINATES
C            ICHP               P SCRATCH ARRAY FOR PIP TEST
C
C **********************************************************************
C
      SUBROUTINE LABMOV(MBR,NVERT,IDTYP,XCENT,YCENT,IER)
      COMMON /IO/ NPRNT,IOIN
$Include:'xypnts.inc'
$Include:'pqpnts.inc'
      REAL MBR(4),NMBR(4),XMIN,XMAX,YMIN,YMAX
C
C  GET CURRENT DISPLAY WINDOW
C
      CALL WHWND(XMIN,YMIN,XMAX,YMAX)
C
C  BRANCH ON DATA TYPE
C
      IF (IDTYP .EQ. 1  .OR.  IDTYP .EQ. 11) GO TO 90000
      IF (IDTYP .EQ. 3  .OR.  IDTYP .EQ. 13) GO TO 200
      IF (IDTYP .NE. 2  .AND. IDTYP .NE. 12) GO TO 90000
C
C  FIND THE A POINT ON THE LINE IN THE WINDOW
C
      ISTRT = 0
      ISTOP = 0
      DO 160 I=1,NVERT
        call spoints('X ',x,xp,i,1)
        call spoints('Y ',y,yp,i,1)
c        XP = X(I)
c        YP = Y(I)
        IF (XP .LT. XMIN .OR. XP .GT. XMAX) GO TO 140
        IF (YP .LT. YMIN .OR. YP .GT. YMAX) GO TO 140
C  SET THE POINT AS BEING INSIDE
          IF (ISTRT .EQ. 0) ISTRT = I
          ISTOP = I
          GO TO 160
 140  IF (ISTOP .NE. 0) GO TO 180
 160  CONTINUE
C
C  COULD NOT FIND ANY INSIDE
C
      GO TO 90000
C
C  LABEL BETWEEN THE START AND STOP OF THE FIRST SEGMENT INSIDE
C
 180  CONTINUE
      I = ISTRT + (ISTOP - ISTRT) / 2
      call spoints('X ',x,xcent,i,1)
      call spoints('Y ',y,ycent,i,1)
c      XCENT = X(I)
c      YCENT = Y(I)
      GO TO 90000
C
C  FOR POLYGONS RESET IT'S WINDOW & DETERMINE WHICH SIDE THE LABEL FALLS ON
C
 200  CONTINUE
      NMBR(1) = MBR(1)
      IF (NMBR(1) .LT. XMIN) NMBR(1) = XMIN
      NMBR(2) = MBR(2)
      IF (NMBR(2) .GT. XMAX) NMBR(2) = XMAX
      NMBR(3) = MBR(3)
      IF (NMBR(3) .LT. YMIN) NMBR(3) = YMIN
      NMBR(4) = MBR(4)
      IF (NMBR(4) .GT. YMAX)  NMBR(4) = YMAX
C
C  ADJUST THE X VALUE
C
      IF(XCENT.LT.NMBR(1)) XCENT = NMBR(1)+(NMBR(2)-NMBR(1))*.4
      IF(XCENT.GT.NMBR(2)) XCENT = NMBR(2)-(NMBR(2)-NMBR(1))*.6
C
C  NOW ADJUST THE Y VALUE
C
      IF(YCENT.LT.NMBR(3)) YCENT = NMBR(3)+(NMBR(4)-NMBR(3))*.5
      IF(YCENT.GT.NMBR(4)) YCENT = NMBR(4)-(NMBR(4)-NMBR(3))*.5
C
C  SEE IF POINT IS IN POLYGON WHILE FINDING CLOSEST POINT FOR PIP FAILURE
C
             DIST = 9999999.0
             ICLOSE = 1
             IPIP = 1
             INT = 0
             XP = XCENT
             YP = YCENT
             DO 350 I=1,NVERT
               TEST = 0.
               call spoints('X ',x,xi,i,1)
               IF (XI .GE. XP) TEST = 1.
c                         p(i)=test
                call spoints('P ',p,test,i,2)
 350         CONTINUE
C
             DO 360 I=1,NVERT
               J=I+1
               IF(I.EQ.NVERT) J = 1
               call spoints('X ',x,xi,i,1)
               call spoints('X ',x,xj,j,1)
               call spoints('Y ',y,yi,i,1)
               call spoints('Y ',y,yj,j,1)
               call spoints('P ',p,pi,i,1)
               call spoints('P ',p,pj,j,1)
               IF(PI.EQ.PJ) GO TO 358
               IF(YI.GE.YP  .AND.  YJ.GE.YP) GO TO 355
               IF(YI.LT.YP  .AND.  YJ.LT.YP) GO TO 358
               YINT = YI+(XP-XI)*(YJ-YI)/(XJ-XI)
               IF (YINT.LT.YP) GO TO 358
 355           INT=INT+1
C  FIND CLOSEST IN WINDOW
 358           CONTINUE
               X1 = XI
               Y1 = YI
      IF (INVWND(X1,X1,Y1,Y1,XMIN,XMAX,YMIN,YMAX) .EQ. 0) GO TO 360
      DSQ = SQRT( (ABS(X1-XP)*ABS(X1-XP)) + (ABS(Y1-YP)*ABS(Y1-YP)) )
               IF (DSQ .LT. DIST) DIST = DSQ
               IF (DSQ .EQ. DIST) ICLOSE = I
 360         CONTINUE
             INTD2=INT/2
             INT2=2*INTD2
             IF(INT2.EQ.INT) IPIP=0
C  IF INSIDE THEN QUIT PROCESSING
             IF (IPIP .EQ. 1) GO TO 90000
C
C  POINT NOT INSIDE...DRAW A LINE TO IT
C
           CALL TKPLOT(X(ICLOSE),Y(ICLOSE),3)
           CALL TKPLOT(XP,YP,2)
C
C  JUMP OUT
C
90000 CONTINUE
      RETURN
      END
