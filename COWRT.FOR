        SUBROUTINE COWRT(IARR,NVERT,NREC,XXMIN,YYMIN,ICHAN,IZ)
$Include:'io.inc'
        COMMON /CSTFC/  XMAX,YMAX,SCALE,XMIN,YMIN
$Include:'xypnts.inc'
        DIMENSION IR3(2),IT(2),IT1(2)
        DIMENSION IARR(128),IR1(2),IR2(2)
        DOUBLE PRECISION RANGE,DSCAL
        INTEGER WHOLE, PARTY
        EQUIVALENCE (IT(1),XT),(YT,IT1(1))
        EQUIVALENCE(IR3(1),SCALE)
        EQUIVALENCE (XMAX,IR1(1)),(YMAX,IR2(1))
C
C *****************************************************************
C
C             S U B R O U T I N E    C O W R T
C
C  FUNCTION - WRITE A COORDINATE STRING TO DISK
C
C  INPUT PARAMETERS:
C       IBUFF  - 128 WORD WORK SPACE
C       NVERT - NUMBER OF COORDINATE PAIRS IN ITEM
C       NREC  - RECORD POINTTER
C                   IN = CURRENT POSITION
C                  OUT = POSITION AFTER COORDINATE READ
C       XMIN  - MIN X OF ITEM
C       YMIN  - YMIN OF ITEM
C      ICHAN  - CHANNEL WHICH X Y POINTS ARE READ FROM, WRITTEN TO
C
C ******************************************************************
C
C
C  SET 2D/3D COUNTER
C
C       Ydiff=0.0
       XMIN=XXMIN
       YMIN=YYMIN
       KPAR=64
       IF(IZ.EQ.1)KPAR=42
        IF(IZ.EQ.2) KPAR=32
        IR1(1)=IARR(17)
        IR1(2)=IARR(18)
        IR2(1)=IARR(21)
C
        ILNDS=IARR(26)
        IR2(2)=IARR(22)
C
        XLEN=(ABS(XMAX)-ABS(XMIN))
       YLEN=ABS(YMAX)-ABS(YMIN)
       RANGE=DBLE(AMAX1(XLEN,YLEN))
       DSCAL=RANGE/32000.D0
        SCALE=SNGL(DSCAL)
       if(scale.eq.0.0)scale=1.0
       IARR(127)=IR3(1)
       IARR(128)=IR3(2)
C
        INC=1
        IF(ILNDS.EQ.0) GO TO 78
        INC=((ILNDS-99)/128)+1
        IF(ILNDS.GT.99) INC=INC+1
  78    CONTINUE
        CALL WRBLK(ICHAN,NREC-INC,IARR,1,IER)
        IF (IER .NE. 0) WRITE(NPRNT,3000) NREC-INC
C
  5     CONTINUE
        WHOLE=NVERT/KPAR
        PARTY=NVERT-(WHOLE*KPAR)
        IPOINT=0
        CALL CLEAR(IARR,128)
        IF(WHOLE.LE.0) GO TO 25
C
        DO 20 I=1,WHOLE
           IC=0
           DO 10 J=1,KPAR
C
C  GET POINT
C
              IPOINT=IPOINT+1
              call spoints('X ',x,xt,ipoint,1)
              call spoints('Y ',y,yt,ipoint,1)
c              XT=X(IPOINT)
c              YT=Y(IPOINT)
C              yt2=yt
C              IF(IZ.EQ.1) ZT=YR(IPOINT)
C
              IF(IZ.EQ.2) GO TO 7
              XT=(XT-XMIN)/SCALE
              YT=(YT-YMIN)/SCALE
              if(scale.eq.0.0)then
               xt=0.0
               yt=0.0
              endif
              IC=IC+1
              IARR(IC)=IFIX(XT)
              IC=IC+1
              IARR(IC)=IFIX(YT)
C             IF(IZ.EQ.0) GO TO 10
C             IC=IC+1
C             IARR(IC)=IFIX(ZT)
              GO TO 10
  7           CONTINUE
              IC=IC+1
              IARR(IC)=IT(1)
              IC=IC+1
              IARR(IC)=IT(2)
              IC=IC+1
              IARR(IC)=IT1(1)
              IC=IC+1
              IARR(IC)=IT1(2)
  10       CONTINUE
C
           CALL WRBLK(ICHAN,NREC,IARR,1,IER)
           IF (IER .NE. 0) WRITE(NPRNT,3000) NREC
           NREC=NREC+1
  20    CONTINUE
C
C
  25    continue
        IF(PARTY.EQ.0) RETURN
C
        IC=0
        DO 30 I=1,PARTY
        IPOINT=IPOINT+1
              call spoints('X ',x,xt,ipoint,1)
              call spoints('Y ',y,yt,ipoint,1)
c        XT=X(IPOINT)
c        YT=Y(IPOINT)
C        yt2=yt
        IF(IZ.EQ.2) GO TO 17
C       IF(IZ.EQ.1) ZT=YR(IPOINT)
        XT=(XT-XMIN)/SCALE
        YT=(YT-YMIN)/SCALE
        if(scale.eq.0.0)then
         xt=0.0
         yt=0.0
        endif
        IC=IC+1
        IARR(IC)=ANINT(XT)
        IC=IC+1
        IARR(IC)=ANINT(YT)
C        IF(IZ.EQ.0) GO TO 30
C        IC=IC+1
C        IARR(IC)=IFIX(ZT)
        GO TO 30
  17    CONTINUE
        IC=IC+1
        IARR(IC)=IT(1)
        IC=IC+1
        IARR(IC)=IT(2)
        IC=IC+1
        IARR(IC)=IT1(1)
        IC=IC+1
        IARR(IC)=IT1(2)
C
  30    CONTINUE
C
        CALL WRBLK(ICHAN,NREC,IARR,1,IER)
        IF (IER .NE. 0) WRITE(NPRNT,3000) NREC
        NREC = NREC + 1
C
C  ERROR RETURN
C
3000   FORMAT(' *COWRT*  ERROR FROM WRBLK OF RECORD',I5)
C
C  AND RETURN
C
        RETURN
        END
