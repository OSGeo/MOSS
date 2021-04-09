C******************************************************************************
C
C     NAME:         SUBROUTINE BLML
C
C     FUNCTION:     PRODUCES THE BLM LINES
C
C                   THERE ARE THREE TYPES OF CALLS FOR THE ROUTINE:
C                   1.INITIALIZATION - OPEN THE LINE FONT FILE...LTYP
C                       IS NEGATIVE, THE CHANNEL NUMBER USED IS THE ABSOLUTE
C                       VALUE OF LTYP, THE VALUE OF IDVC IS THE OUTPUT FILE
C                       FOR ERROR MESSAGE.
C                   2.PLOTTING - PLOT LINE(S).....LTYP IS POSITIVE
C                       PLOT THE LINE(S) USING INDEX NUMBER 'LTYP'
C                   3.TERMINATION - CLOSE THE LINE FONT FILE...LTYP EQUALS -999
C                       CLOSE THE CHANNEL TO THE LINE FONT FILE
C
C     PARAMETERS:
C                  IDVC    I       I*2
C                  THE DEVICE CODE
C
C                  NPTS    I       I*2
C                  NUMBER OF X,Y COORDINATE PAIRS
C
C                  WIDE    I       R*4
C                  WIDTH IN INCHES OF CORRIDOR FOR OTHER THAN SINGLE LINE
C                  TYPES (THE DISTANCE BETWEEN PARALLEL LINES)
C
C                  DPOST  I         R*4
C                  THE INCREMENT BETWEEN SYMBOLS  X......X.......X
C
C                  LTYP    I         I*2
C                  THE TYPE OF LINE DESIRED  RANGE IS ONE TO THIRTY-FOUR
C
C                  IER     O         I*2
C                  RETURNED ERROR CODE  NOT 1 THEN ERROR
C
C     SUBROUTINES REQUIRED: BLMLD HSYMB X & Y FUNCTIONS
C
C*******************************************************************************
      SUBROUTINE BLML(IDVC,NPTS,WIDE,DPOST,LTYP,IER)
      COMMON /BLMLSC/ ICHAN,IP(3,10),LCODE
$Include:'xypnts.inc'
        DIMENSION YA(2,5),iwrk(25),ihold(2)
        CHARACTER*25 IFILER
        EQUIVALENCE (IFILER,IWRK(1))
        DATA IOUT/6/, PI/3.1415926/
C
      XX(II)=FLOAT(II)*DMK/200.
      YY(II)=FLOAT(II)*WIDE/200.

        IER = 0
C
C.......BRANCH ON OPEN OR CLOSE CALL OR REGULAR LINE CALL........
        IF(LTYP.GE.0) GO TO 2
        IF(LTYP.EQ.-999) GO TO 1
C
C.......OPEN LFONTS FILE............
        ICHAN = 56
        IF(IDVC.EQ.10 .OR. IDVC.EQ.12) IOUT = IDVC
        IFILER='\GIS\INTRFACE\BLMLINE.DT'
        call openf(ichan,iwrk,1,64,2,ier)
        if(ier.ne.0)go to 900
        GO TO 90000
C
C.......CLOSE .DT FILE............
 1      CLOSE ( ICHAN, iostat=IER )
        GO TO 90000
C
C.......PROCESS LINE(S).........
 2      IF(LTYP.GE.6) GO TO 4
         IPEN=3
         DO 3 I=1,NPTS
         call spoints('X ',x,xxrr,i,1)
         call spoints('Y ',y,yyrr,i,1)
         CALL BLMLD(IOUT,xxrr,yyrr,IPEN)
         IPEN=2
         IF(LTYP.NE.5) IPEN=3+LTYP
 3       CONTINUE
         GO TO 90000
C.......LINES WITH CODES GE 6........
 4       TOTLEN=0.
        ICFNT = 12
        ISFNT = 8
         DO 5 I=2,NPTS
         call spoints('X ',x,xi,i,1)
         call spoints('X ',x,xi1,i-1,1)
         call spoints('Y ',y,yi,i,1)
         call spoints('Y ',y,yi1,i-1,1)
 5       TOTLEN=TOTLEN + SQRT((xi-xi1)**2 + (yi-yi1)**2)
        NPOSTS=TOTLEN/DPOST + .5
        IF(NPOSTS.EQ.0) NPOSTS=1
        DMARK=TOTLEN/NPOSTS
        DMK=DPOST
C.......READ LINE CODE RECORD......
        IF(LTYP .EQ. LCODE) GO TO 8
        READ(ICHAN,REC=LTYP,ERR=910,iostat=ier) IP
        LCODE = LTYP
 8      I=0
 6      I=I+1
        ihold(1)=ip(1,I)
        call unpack(ihold,1)
        IF(I.GT.10) GO TO 920
C.......GET PEN CODE.....
C           KK=BYTE(IP(1,I),1)
        KK=ihold(1)
        K=KK/10
        IPEN=KK-K*10
C.......IF K = 0, DONE WITH THIS LINE.....
        IF(K.EQ.0) GO TO 100
C.......GET LINE TYPE......
C           ILT=BYTE(IP(1,I),2)
        ilt=ihold(2)
        IX=IP(2,I)
        IY=IP(3,I)
C
C.......BRANCH ACCORDING TO K TYPE.....
        GO TO (10,20,30,40,40),K
C
C......CONTINUOUS LINE
 10    ITYP=2
       IF(ILT.NE.5) ITYP=ILT+3
C.......BRANCH IF YOFFSET.....
       IF(IY.NE.0) GO TO 19
        JPEN=3
C.......IF IX NE 0, NOT A CONTINUOUS LINE....
        IF(IX.NE.0) GO TO 12
         DO 11 J=1,NPTS
         call spoints('X ',x,xj,j,1)
         call spoints('Y ',y,yj,j,1)
         CALL BLMLD(IOUT,xj,yj,JPEN)
 11      JPEN=ITYP
         GO TO 6
C
C.......INTERMITTENT CENTERLINE
 12      DMK=DPOST
         DLEN=XX(IX)
         DL=DLEN
         call spoints('X ',x,xc,1,1)
         call spoints('Y ',y,yc,1,1)
c         XC=X(1)
c         YC=Y(1)
         LPEN=ITYP+3
         KPEN=ITYP
         CALL BLMLD(IOUT,XC,YC,3)
         DO 15 J=2,NPTS
         call spoints('X ',x,xj,j,1)
         call spoints('Y ',y,yj,j,1)
c         XJ=X(J)
c         YJ=Y(J)
 13      D=SQRT((XC-XJ)**2 + (YC-YJ)**2)
         IF(D.GE.DL) GO TO 14
         DL=DL-D
         XC=XJ
         YC=YJ
         CALL BLMLD(IOUT,XC,YC,KPEN)
          GO TO 15
 14       XP=XC + (XJ-XC)*DL/D
          YP=YC + (YJ-YC)*DL/D
         CALL BLMLD(IOUT,XP,YP,KPEN)
         KPEN=LPEN-KPEN
         DLEN=DMK-DLEN
         DL=DLEN
         XC=XP
         YC=YP
          GO TO 13
  15      CONTINUE
          GO TO 6
C
C.......CONTINUOUS OFFSET LINE....
C.......DIST = OFFSET DISTANCE....
 19      DIST=YY(IY)
         DLEN=ABS(XX(IX))
         DL=DLEN
         LOW=0
 135     LOW=LOW+1
C.......ALL POINTS SAME IF LOW EQ NPTS.....
         IF(LOW.EQ.NPTS) GO TO 6
          call spoints('X ',x,xlow,low,1)
          call spoints('Y ',y,ylow,low,1)
          call spoints('X ',x,xlow1,low+1,1)
          call spoints('Y ',y,ylow1,low+1,1)
          IF(XLOW.EQ.XLOW1 .AND. YLOW.EQ.YLOW1) GO TO 135
          ANG=ATAN2(YLOW1-YLOW,XLOW1-XLOW)
          XP=XLOW + DIST*SIN(-ANG)
          YP=YLOW + DIST*COS(-ANG)
          CALL BLMLD(IOUT,XP,YP,3)
          XC=XLOW
          YC=YLOW
          LPEN=ITYP+3
          KPEN=ITYP
           IF(IX.LT.0) KPEN=LPEN-KPEN
           IF(LOW.GE.NPTS-1) GO TO 146
           DO 145 J=LOW+1,NPTS-1
            call spoints('X ',x,x1,j-1,1)
            call spoints('Y ',y,y1,j-1,1)
            call spoints('X ',x,x2,j,1)
            call spoints('Y ',y,y2,j,1)
            call spoints('X ',x,x3,j+1,1)
            call spoints('Y ',y,y3,j+1,1)
c            X1=X(J-1)
c            Y1=Y(J-1)
c            X2=X(J)
c            Y2=Y(J)
c            X3=X(J+1)
c            Y3=Y(J+1)
           IF(X1.EQ.X2 .AND. Y1.EQ.Y2) GO TO 145
        IF(X3.EQ.X2 .AND. Y3.EQ.Y2) GO TO 145
        ANG1=ATAN2(Y2-Y1,X2-X1) + PI/2.
        ANG2=ATAN2(Y3-Y2,X3-X2) + PI/2.
        IF(ANG1.GE.PI) ANG1=ANG1-2.*PI
        IF(ANG2.GE.PI) ANG2=ANG2-2.*PI
        IF(ANG1.LT.0.) ANG1=ANG1+2.*PI
        IF(ANG2.LT.0.) ANG2=ANG2+2.*PI
        IF(ANG1.LT.PI/2. .AND. ANG2.GT.3.*PI/2.) ANG2=ANG2-2.*PI
        IF(ANG2.LT.PI/2. .AND. ANG1.GT.3.*PI/2.) ANG1=ANG1-2.*PI
        ANG=(ANG1+ANG2)/2.
        XP=X2 + DIST*COS(ANG)
        YP=Y2 + DIST*SIN(ANG)
C.......NORMAL OFFSET CONTINUOUS LINE......
        IF(IX.EQ.0) GO TO 140
C
C.......OFFSET BROKEN LINE
 120     D=SQRT((XC-X2)**2 + (YC-Y2)**2)
         IF(D.GE.DL) GO TO 124
         DL=DL-D
         XC=X2
         YC=Y2
         GO TO 140
 124     XD=(X2-XC)*DL/D
         YD=(Y2-YC)*DL/D
          CALL BLMLD(IOUT,XP+XD,YP+YD,KPEN)
           KPEN=LPEN-KPEN
           DLEN=DMK-DLEN
           DL=DLEN
           XC=XC+XD
           YC=YC+YD
           GO TO 120
 140        CALL BLMLD(IOUT,XP,YP,KPEN)
 145       CONTINUE
 146     call spoints('X ',x,xnpts,npts,1)
         call spoints('X ',x,xnpts1,npts-1,1)
         call spoints('Y ',y,ynpts,npts,1)
         call spoints('Y ',y,ynpts1,npts-1,1)
         IF(XNPTS.EQ.XNPTS1 .AND. YNPTS.EQ.YNPTS1) GO TO 147
         ANG=ATAN2(YNPTS-YNPTS1,XNPTS-XNPTS1)
         XP=XNPTS + DIST*SIN(-ANG)
         YP=YNPTS + DIST*COS(-ANG)
         GO TO 148
 147     XP=XNPTS
         YP=YNPTS
 148     CALL BLMLD(IOUT,XP,YP,ITYP)
         GO TO 6
C
C.......EVERY DMARK
 20      CONTINUE
         GO TO 6
C          SPECIAL CODE #3
 30      NUM=ILT
          YA(1,1)=YY(IX)
          YA(2,1)=YY(IY)
          N=1
 31       IF(N.GE.NUM) GO TO 32
          I=I+1
           YA(1,N+1)=YY(IP(2,I))
           YA(2,N+1)=YY(IP(3,I))
           N=N+1
          GO TO 31
 32        N=1
           KPEN=3
           IX=0
C
C.......CENTERED SYMBOL OR CHARACTER EVERY DMARK
 40       CONTINUE
          DMK=DMARK
          IF(K.NE.3) DMK=DPOST
           DL=XX(IX)
           call spoints('X ',x,xc,1,1)
           call spoints('Y ',y,yc,1,1)
c           XC=X(1)
c           YC=Y(1)
          DO 48 J=2,NPTS
           call spoints('X ',x,xj,j,1)
           call spoints('Y ',y,yj,j,1)
41           D=SQRT((XC-XJ)**2 + (YC-YJ)**2)
           IF(D.GE.DL) GO TO 42
            DL=DL-D
            XC=XJ
            YC=YJ
           GO TO 48
 42         XP=XC + (XJ-XC)*DL/D
            YP=YC + (YJ-YC)*DL/D
            ANG=ATAN2(YJ-YC,XJ-XC)
            XC=XP
            YC=YP
             DL=DMK
            SA=SIN(-ANG)
            CA=COS(-ANG)
            IF(K.NE.3) GO TO 47
C
C.......SPECIAL CODE #3
            CALL BLMLD(IOUT,XC+YA(1,N)*SA,YC+YA(1,N)*CA,KPEN)
            KPEN=2
            CALL BLMLD(IOUT,XC+YA(2,N)*SA,YC+YA(2,N)*CA,KPEN)
             N=N+1
            IF(N.GT.NUM) N=1
            GO TO 41
 47          DY=0.
C.......CENTERED SYMBOL....
            IF(K.EQ.4) DY=YY(IY)
C.......CHARACTER.....
            IF(K.EQ.5) DY=-WIDE/2.
            XPP=XC + DY*SA
            YPP=YC + DY*CA
             ANG=ANG/.0174533
C              BYTE(ITYP,1) = ILT
      IF(K.EQ.4) CALL plotsym(IDVC,ISFNT,XPP,YPP,WIDE,WIDE,ILT,
     1 ANG,0.0,0.0,0,1,IER)
      IF(K.EQ.5) CALL plotsym(IDVC,ICFNT,XPP,YPP,WIDE,WIDE,ITYP,
     1 ANG,0.0,0.0,0,1,IER)
        IF(IER .NE. 0) GO TO 930
           GO TO 41
 48     CONTINUE
         GO TO 6
 100     CONTINUE
       GO TO 90000
C
C.......ERROR RETURNS.....
 900   WRITE(IOUT,89001)IER
89001   FORMAT(' *BLML*    ERROR OPENING LFONTS   ERROR',I5)
        GO TO 90000
C
 910    WRITE(IOUT,89100)LTYP,IER
89100 FORMAT(' *BLML*    ERROR READING LFONTS WITH LTYP ',I5,
     + ' ERROR ',I5)
        GO TO 90000
C
 920    IER = -1
        WRITE(IOUT,89020)LTYP
89020 FORMAT(' *BLML*    GT 10 DESCRIBING LINES IN LFONTS FILE  LTYP=',
     +  I5)
        GO TO 90000
C
 930    CONTINUE
        WRITE(IOUT,89030)IER
89030   FORMAT(' *BLML*    ERROR FROM HSYMB   ERROR ',I5)
        GO TO 90000
C
89000   CONTINUE
C
90000   CONTINUE
         RETURN
         END
