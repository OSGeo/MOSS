C***********************************************************************
C          DRAW AND LABEL CONTOUR LINES
C
C          (X,Y)    = DISK ARRAYS OF (X,Y) POINTS OF LINE
C          NPS      = # OF POINTS IN (X,Y)
C          LABL     = ARRAY CONTAINING LABEL CHARACTERS (2 PER WORD)
C          NCHAR    = # CHARS IN LABEL
C          SIZE     = HEIGHT OF LABELS
C***********************************************************************
        SUBROUTINE CONTPL(NPS,LABL,SIZE)
        DIMENSION LABL(1),ich(2)
C        INTEGER BYTE
$Include: 'xypnts.inc'
        NCHAR=0 
C/*  ELIMINATE BLANKS FROM SUBJECT
        DO 5 I=1,30
        call byteit(labl(i),ich)
        do 55 lll=1,2 
C        ICH=BYTE(LABL,I)
        IF(NCHAR.EQ.0) GO TO 4
        IF(ICH(lll).EQ.ICHAR(' ')) GO TO 6 
C/*  1st BLANK ENDS SUBJECT
 3       NCHAR=NCHAR+1
C        CALL BYTSET(LABL,NCHAR,ICH)
        GO TO 55
 4      IF(ICH(lll).NE.ICHAR(' ')) GO TO 3
C/*  1st NON-BLANK
 55     continue
 5      CONTINUE
C          LOCATE LABEL AT CENTER OF LINE
 6      SIZELAB=SIZE*(NCHAR+1)  
C/*  LENGTH OF LABEL
C          FIND LINE LENGTH
        XLEN=0.
        DO 10 I=2,NPS
 10     XLEN=XLEN + SQRT((X(I)-X(I-1))**2 + (Y(I)-Y(I-1))**2)
        HLEN=XLEN/2.
C/*  CENTER OF LINE
        IF(SIZELAB.LT.HLEN) GO TO 40
C          LABEL TOO LONG
 15     CALL SYMBOL(X(NPS),Y(NPS),SIZE,LABL,0.,NCHAR)
        IPEN=3
        DO 35 I=1,NPS
        CALL PLOTW(X(I),Y(I),IPEN)
 35     IPEN=2
        GO TO 150
 40     HLEN=HLEN-SIZELAB/2.
C/*  DIST ALONG LINE TO START LABEL
        XLEN=0.
        CALL PLOTW(X(1),Y(1),3)
        DO 20 I=2,NPS
        DL=SQRT((X(I)-X(I-1))**2 + (Y(I)-Y(I-1))**2)
        XLEN=XLEN+DL
        IF(XLEN.GT.HLEN) GO TO 30
C/*  BEYOND START MARK
        CALL PLOTW(X(I),Y(I),2)
 20     CONTINUE
 30     XLEN=XLEN-DL
        FRAC=(HLEN-XLEN)/DL
        XC=X(I-1) + (X(I)-X(I-1))*FRAC
        YC=Y(I-1) + (Y(I)-Y(I-1))*FRAC
        NDEX=I
C          FIND (XP,YP) SUCH THAT DIST (XC,YC)-(XP,YP) = SIZELAB
        CALL PLOTW(XC,YC,2)
        DO 50 I=NDEX,NPS
        DL=SQRT((X(I)-XC)**2 + (Y(I)-YC)**2)
        IF(DL.GT.SIZELAB) GO TO 60
C/*  (XP,YP) BEFORE THIS POINT
 50     CONTINUE
        GO TO 15
C          (XP,YP) IS BETWEEN (X(NN),Y(NN)) & (X(NN+1),Y(NN+1))
 60     NN=I
        X1=X(NN-1)
        Y1=Y(NN-1)
        X2=X(NN)
        Y2=Y(NN)
        DO 70 I=1,15 
C/*  ITERATE
        XP=(X1+X2)/2.
        YP=(Y1+Y2)/2.
        D=SQRT((XP-XC)**2 + (YP-YC)**2)
        IF(D.LT.SIZELAB) GO TO 65
        X2=XP
        Y2=YP
        GO TO 70
 65     X1=XP
        Y1=YP
 70     CONTINUE
        DX=XP-XC
        DY=YP-YC
        ANG=ATAN2(DY,DX)/.0174533
        FACT=(NCHAR+1)*2
        DX=DX/FACT
        DY=DY/FACT
        IF(ANG.GT.90. .OR. ANG.LT.-90.) GO TO 80
        A=(ANG-90.)*.0174533
        XC=XC+DX+COS(A)*SIZE/2.
        YC=YC+DY+SIN(A)*SIZE/2.
        CALL SYMBOL(XC,YC,SIZE,LABL,ANG,NCHAR)
        GO TO 90
 80     ANG=ANG+180.
        A=(ANG-90.)*.0174533
        XC=XP-DX + COS(A)*SIZE/2.
        YC=YP-DY + SIN(A)*SIZE/2.
        CALL SYMBOL(XC,YC,SIZE,LABL,ANG,NCHAR)
 90     CALL PLOTW(XP,YP,3)
        DO 100 I=NN,NPS
 100    CALL PLOTW(X(I),Y(I),2)
 150    RETURN
        END
