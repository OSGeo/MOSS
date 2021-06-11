        SUBROUTINE BLMLINE(X,Y,NPTS,WIDE,DPOST,ICODE,IPEN1,IPEN2)
        INTEGER ipip(2),maps(12)
        CHARACTER LINEFILE*21
        DIMENSION X(1),Y(1),YA(2,5), IPP(32)
C        COMMON /CHANL/  IO(40)
        COMMON /BLMLSC/ IOPEN,LU,IP(3,10)
        EQUIVALENCE     (IP,IPP),(LINEFILE,MAPS(1))
        DATA PI/3.1415926/
c        data maps/2HBL,2HML,2HIN,2HE.,2HDT/
                XX(II)=FLOAT(II)*DMK/200.
                YY(II)=FLOAT(II)*WIDE/200.
        IF(ICODE.LE.0) RETURN
        IF(ICODE.GE.6) GO TO 2
        IF(IPEN1.GT.0) CALL NEWPEN(IPEN1)
        IPEN=3
        DO 1 I=1,NPTS
        CALL COMDASH(X(I),Y(I),IPEN)
        IPEN=2
        IF(ICODE.NE.5) IPEN=3+ICODE
 1      CONTINUE
        RETURN
 2      TOTLEN=0.
        DO 3 I=2,NPTS
 3      TOTLEN=TOTLEN + SQRT((X(I)-X(I-1))**2 + (Y(I)-Y(I-1))**2)
        NPOSTS=TOTLEN/DPOST + .5
        IF(NPOSTS.EQ.0) NPOSTS=1
        DMARK=TOTLEN/NPOSTS
        DMK=DPOST
C
        LINEFILE='\GIS\intrface\BLMLINE.DT'
        IF(IOPEN.EQ.0) THEN
          CALL OPENF(48,maps,1,64,2,IER)
          IF ( IER.NE.0 ) GO TO 900
        ENDIF
        IOPEN=1
        READ(48,REC=ICODE,ERR=910,END=910)IPP
C        CALL RDBLK(48,ICODE,IP,1,IIER)
        LASTPEN=0
        I=0
 5      I=I+1
        IF(I.GT.10) GO TO 920
        call byteit(ip(1,i),ipip)
        KK=ipip(1)
C/*  GET PEN CODE
        K=KK/10
        IPEN=KK-K*10
        IF(K.EQ.0) GO TO 100
C/*  DONE WITH THIS SYMBOL
        IF(IPEN.EQ.LASTPEN) GO TO 6
        IF(IPEN.EQ.1 .AND. IPEN1.EQ.0) GO TO 5
C/*  NO PEN
        IF(IPEN.EQ.2 .AND. IPEN2.EQ.0) GO TO 5
C/*  NO PEN
        IF(IPEN.EQ.1 .AND. IPEN1.GT.0) CALL NEWPEN(IPEN1)
        IF(IPEN.EQ.2 .AND. IPEN2.GT.0) CALL NEWPEN(IPEN2)
        LASTPEN=IPEN
 6      ILT=ipip(2) 
C/*  GET LINE TYPE (1-5)
        IX=IP(2,I)
        IY=IP(3,I)
        GO TO (10,20,30,40,40),K
C          CONTINUOUS LINE
 10     ITYP=2
        IF(ILT.NE.5) ITYP=ILT+3
C/*  PEN DOWN CODE FOR COMDASH
        IF(IY.NE.0) GO TO 19
C/*  OFFSET LINE
        JPEN=3
        IF(IX.NE.0) GO TO 12
C/*  NOT A CONTINUOUS LINE
        DO 11 J=1,NPTS
        CALL COMDASH(X(J),Y(J),JPEN)
 11     JPEN=ITYP
        GO TO 5
C          INTERMITTENT CENTERLINE
 12     DMK=DPOST
        DLEN=XX(IX)
        DL=DLEN
        XC=X(1)
        YC=Y(1)
        LPEN=ITYP+3
        KPEN=ITYP
        CALL COMDASH(XC,YC,3)
        DO 15 J=2,NPTS
        XJ=X(J)
        YJ=Y(J)
 13     D=SQRT((XC-XJ)**2 + (YC-YJ)**2)
        IF(D.GE.DL) GO TO 14
        DL=DL-D
        XC=XJ
        YC=YJ
        CALL COMDASH(XC,YC,KPEN)
        GO TO 15
 14     XP=XC + (XJ-XC)*DL/D
        YP=YC + (YJ-YC)*DL/D
        CALL COMDASH(XP,YP,KPEN)
        KPEN=LPEN-KPEN
        DLEN=DMK-DLEN
        DL=DLEN
        XC=XP
        YC=YP
        GO TO 13
 15     CONTINUE
        GO TO 5
C          CONTINUOUS OFFSET LINE
 19     DIST=YY(IY)
C/*  OFFSET DISTANCE
        DLEN=ABS(XX(IX))
        DL=DLEN
        LOW=0
 135    LOW=LOW+1
        IF(LOW.EQ.NPTS) GO TO 5
C/*  ALL POINTS THE SAME
        IF(X(LOW).EQ.X(LOW+1) .AND. Y(LOW).EQ.Y(LOW+1)) GO TO 135
        ANG=ATAN2(Y(LOW+1)-Y(LOW),X(LOW+1)-X(LOW))
        XP=X(LOW) + DIST*SIN(-ANG)
        YP=Y(LOW) + DIST*COS(-ANG)
        CALL COMDASH(XP,YP,3)
        XC=X(LOW)
        YC=Y(LOW)
        LPEN=ITYP+3
        KPEN=ITYP
        IF(IX.LT.0) KPEN=LPEN-KPEN
        IF(LOW.GE.NPTS-1) GO TO 146
        DO 145 J=LOW+1,NPTS-1
        X1=X(J-1)
        Y1=Y(J-1)
        X2=X(J)
        Y2=Y(J)
        X3=X(J+1)
        Y3=Y(J+1)
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
        IF(IX.EQ.0) GO TO 140
C/*  NORMAL OFFSET CONTINUOUS LINE
C          OFFSET BROKEN LINE
 120    D=SQRT((XC-X2)**2 + (YC-Y2)**2)
        IF(D.GE.DL) GO TO 124
        DL=DL-D
        XC=X2
        YC=Y2
        GO TO 140
 124    XD=(X2-XC)*DL/D
        YD=(Y2-YC)*DL/D
        CALL COMDASH(XP+XD,YP+YD,KPEN)
        KPEN=LPEN-KPEN
        DLEN=DMK-DLEN
        DL=DLEN
        XC=XC+XD
        YC=YC+YD
        GO TO 120
 140    CALL COMDASH(XP,YP,KPEN)
 145    CONTINUE
 146    IF(X(NPTS).EQ.X(NPTS-1) .AND. Y(NPTS).EQ.Y(NPTS-1)) GO TO 147
        ANG=ATAN2(Y(NPTS)-Y(NPTS-1),X(NPTS)-X(NPTS-1))
        XP=X(NPTS) + DIST*SIN(-ANG)
        YP=Y(NPTS) + DIST*COS(-ANG)
        GO TO 148
 147    XP=X(NPTS)
        YP=Y(NPTS)
 148    CALL COMDASH(XP,YP,ITYP)
        GO TO 5
C          EVERY DMARK
 20     CONTINUE
        GO TO 5
C          SPECIAL CODE #3
 30     NUM=ILT
        YA(1,1)=YY(IX)
        YA(2,1)=YY(IY)
        N=1
 31     IF(N.GE.NUM) GO TO 32
        I=I+1
        YA(1,N+1)=YY(IP(2,I))
        YA(2,N+1)=YY(IP(3,I))
        N=N+1
        GO TO 31
 32     N=1
        KPEN=3
        IX=0
C          CENTERED SYMBOL OR CHARACTER EVERY DMARK
 40     CONTINUE
        DMK=DMARK
        IF(K.NE.3) DMK=DPOST
        DL=XX(IX)
        XC=X(1)
        YC=Y(1)
        DO 48 J=2,NPTS
 41     D=SQRT((XC-X(J))**2 + (YC-Y(J))**2)
        IF(D.GE.DL) GO TO 42
        DL=DL-D
        XC=X(J)
        YC=Y(J)
        GO TO 48
 42     XP=XC + (X(J)-XC)*DL/D
        YP=YC + (Y(J)-YC)*DL/D
        ANG=ATAN2(Y(J)-YC,X(J)-XC)
        XC=XP
        YC=YP
        DL=DMK
        SA=SIN(-ANG)
        CA=COS(-ANG)
        IF(K.NE.3) GO TO 47
C          SPECIAL CODE #3
        CALL COMDASH(XC+YA(1,N)*SA,YC+YA(1,N)*CA,KPEN)
        KPEN=2
        CALL COMDASH(XC+YA(2,N)*SA,YC+YA(2,N)*CA,KPEN)
        N=N+1
        IF(N.GT.NUM) N=1
        GO TO 41
 47     DY=0.
        IF(K.EQ.4) DY=YY(IY)
C/*  CENTERED SYMBOL
        IF(K.EQ.5) DY=-WIDE/2.
C/*  CHARACTER
        XPP=XC + DY*SA
        YPP=YC + DY*CA
        ANG=ANG/.0174533
        IF(LASTPEN.EQ.1) IPPEN=IPEN1
        IF(LASTPEN.EQ.2) IPPEN=IPEN2
        IF(K.EQ.4) CALL BLMSYM(XPP,YPP,WIDE,WIDE,ANG,ILT,IPPEN,IPPEN)
        IF(K.EQ.5) CALL SYMBOL(XPP,YPP,WIDE,ILT*256,ANG,1)
        GO TO 41
 48     CONTINUE
        GO TO 5
 100    CONTINUE
        RETURN
 900    WRITE(*,*) 'Could not OPEN BLMLINE.DT'
        RETURN
 910    WRITE(*,*) 'Error READing in BLMLINE   ICODE=',ICODE
        RETURN
 920    WRITE(*,*) '>10 lines in BLMLINE.DT    ICODE=',ICODE
        RETURN
        END
