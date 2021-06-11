C       FUNCTION:   PLOT MAP HEADER  IN LOWER LEFT CORNER OF MAP
C                   PRODUCT.
C
C       PARAMETERS:
C                   ICHAN
C                   CHANNEL TO READ MAP HEADER FROM
C
C                   ICOM
C                   ARRAY HOLDING MAP NAME
C
C
       SUBROUTINE PLOTH(ICHAN,ICOM,I,IDTYPE)
C
C        COMMON /STRNG/ ICARD(80),K
$Include:'strng.inc'
$Include:'xypnts.inc'
        COMMON /LEGAL/ ISHADE,IDASH,ROT,D,MOSAIC,LPEN,LDASH,
     +                 LSTYPE,WIDTHL,DMARK
        COMMON /DIMEN/ X1,X2,Y1,Y2,XLN,YLN,SCALE,FACT,X0,Y0
C        COMMON /CHANL/ IO(40)
        COMMON /PTHKPP/ XNEW1,YNEW1,XNEW2,YNEW2,KPMAP
        COMMON /PENS/   ILPEN,IPPEN,ISPEN,IAPEN
        COMMON /WINDI/  XI1,YI1,XI2,YI2
        COMMON /PENSTP/ PSTOP,IHOLD(6),ICOLR(6),ITHK(6)
        COMMON /ROTATE/ THETAD,TURN,SCAL
        COMMON /SELECT/ IMAPS,ISUB(31),IACT(5,11)
C
        CHARACTER*132 ABC
        character*2 igetit
        LOGICAL PSTOP,TURN
C
        DIMENSION IBUFF(128),ICOM(10),NAM(31),IYR(2),ICR(3),IDUM(2)
        DIMENSION XX(2),YY(2),ISEL(35)
        INTEGER icomm(2),igrab(2)
C        DATA KPMAP/0/,NAM/31*2H  /
C
C   CHECK IF MAPNAMES AND SELECTION CRITERIA ARE TO BE PLOTTED
C
        IF(IMAPS.EQ.ICHAR('N')) GO TO 5
C
C  IF ISHADE = Y AND I LT 0, SKIP EVERYTHING AND RE-CROSSHATCH BOX
C  IN LEGEND
C
        IF(ISHADE.EQ.ICHAR('Y').AND.I.LT.0) GO TO 310
C
C**     READ MAP HEADER INTO IBUFF ARRAY
        CALL RDBLK(ICHAN,1,IBUFF,128,IER)
C
C**     HOW FAR DOWN ON LOWER LEFT ACCORDING TO 1ST 2ND 3RD ETC MAP
C
C**     GET MAP NAME & LENGTH
C
        DO 10 ncc=1,10
          call byteit(icom(ncc),icomm)
          IF(ICOMM(1).LE.ICHAR(' ')) go to 5
          nc=nc+1
         if(icomm(2).le.ichar(' ')) GO TO 5
          nc=nc+1
 10     CONTINUE
C
C**     GET THEME NAME FROM DESCRIPTION
C
  5     WRITE(*,1000)
1000    FORMAT(' Enter LEGEND description [CR=OMIT FROM LEGEND] '\)
        CALL READIN(Inam,31,K,ie)
        if(ie.eq.0)go to 31
C
        CALL PACKC(Inam,NAM,31)
  31    NC0=ie
C
        IF(NC0.EQ.0) GO TO 35
C
C       GET SELECTION CRITERIA FROM COMMON
C
        DO 32 I=1,5
          ISEL(I)=IACT(I,ISUB(31))
  32    CONTINUE
        if(isub(31).ne.3)then
        DO 33 I=1,10
          ISEL(I+5)=ISUB(I)
  33    CONTINUE
        else
         i=0
         j1=10
         do 9090 ll=1,30
          if(isub(ll).ne.0)then
           write(igetit,'(i2)')isub(ll)
           igrab(1)=99
           do 9091 jj=1,2
            if(igetit(jj:jj).gt.' ')then
              igrab(jj)=ichar(igetit(jj:jj))
            endif
 9091     continue
          if(igrab(1).eq.99)then
           i=i+1
           j1=j1+2
           isel(i+5)=32 + ishft(igrab(2),8)
          else
           i=i+1
           j1=j1+1
           isel(i+5)=8224
           i=i+1
           j1=j1+2
           isel(i+5)=igrab(1)+ishft(igrab(2),8)
          endif
          endif
9090    continue
        endif
C
        iselpts=j1 
        KPMAP=KPMAP+1
        YC=(-1.19-((KPMAP)*.19))*FACT
        x9=.11*FACT
        XC=XLN-(4.5*FACT)
C
C  GET COORDINATE SYSTEM
C
        DO 40 J=1,3
          ICR(J)=IBUFF(J+52)
40      CONTINUE
C
C**     PLOT OUT INFORMATION
C
        TURN=.FALSE.
        CALL NEWPEN(1)
        IF(IMAPS.NE.ICHAR('Y')) GO TO 34
        CALL SYMBOL(0.0,YC,x9,ICOM,0.0,NC)
C/*  PLOT MAP NAME
        CALL SYMBOL(12*.11*FACT,YC,x9,ISEL,0.0,iselpts)
C/* PLOT SEL INFO
 34     CALL SYMBOL(XC+.4,YC,x9,NAM,0.0,NC0)
C/* PLOT USER DESCRIPTION
        XNEW1=XLN-(1.0*FACT)
        YNEW1=YC
        XNEW2=XLN
        YNEW2=YC+.14*FACT
 35     IF(.NOT.PSTOP) GO TO 95
C
C        WRITE(ABC,'(A,6A2,A,6A2,A,6A2)') '** PEN CHANGE ** Holder ',
C     +  IHOLD,'Color ',ICOLR,'Thickness ',ITHK
C        CALL OPMES (58,ABC)
 95     IF(NC0.EQ.0) GO TO 999
        IF(IDTYPE.EQ.1) GO TO 100
C/*  SYMBOLS
        IF(IDTYPE.EQ.2) GO TO 200
C/*  LINES
        IF(IDTYPE.EQ.3) GO TO 300
C/*  POLYGONS
        GO TO 999
 100    CALL NEWPEN(ISPEN)
        IF(LSTYPE.LT.0) GO TO 150
C/*  SYMBOL DEFINED IN DATA FILE
        LSYM=LSTYPE
        IF(LSYM.EQ.0) LSYM=8
C/*  DEFAULT = 8 = x9
        CALL BLMSYM((XNEW1+XNEW2)/2.,(YNEW1+YNEW2)/2.,X9,x9,0.,
     +              LSTYPE,ISPEN,ISPEN)
        GO TO 999
 150    CALL SYMBOL(XNEW1,YNEW1,x9,7HSymbols,0.,7)
        GO TO 999
 200    XX(1)=XNEW1
        XX(2)=XNEW2
        YY(1)=(YNEW1+YNEW2)/2.
        YY(2)=YY(1)
        WW=WIDTHL
        IF(WW.GT.x9) WW=X9
        LTY=LSTYPE
        IF(LTY.LT.0) LTY=5 
C/*  CONTOUR LINES
        CALL BLMLINE(XX,YY,2,WW,DMARK,LTY,ILPEN,ISPEN)
        GO TO 999
 300    CALL NEWPEN(IPPEN)
        CALL hpplot(XNEW1,YNEW1,3)
        CALL hpplot(XNEW1,YNEW2,2)
        CALL hpplot(XNEW2,YNEW2,2)
        CALL hpplot(XNEW2,YNEW1,2)
        CALL hpplot(XNEW1,YNEW1,2)
 310    IF(ISHADE.NE.ICHAR('Y')) RETURN
        NPTS=5
          call spoints('X ',x,xnew1,1,2)
c         x(1)=xnew1
          call spoints('Y ',y,ynew1,1,2)
c         y(1)=ynew1 
          call spoints('X ',x,xnew1,2,2) 
c         x(2)=xnew1 
          call spoints('Y ',y,ynew2,2,2)
c         y(2)=ynew2
          call spoints('X ',x,xnew2,3,2)
c         x(3)=xnew2
          call spoints('Y ',y,ynew2,3,2)
c         y(3)=ynew2
          call spoints('X ',x,xnew2,4,2)
c         x(4)=xnew2 
          call spoints('Y ',y,ynew1,4,2)
c         y(4)=ynew1
          call spoints('X ',x,xnew1,5,2)
c         x(5)=xnew1
          call spoints('Y ',y,ynew1,5,2)
c         y(5)=ynew1
        CALL NEWPEN(ILPEN)
        XT1=X1
C/*  CHANGE MIN/MAX FOR CRHTCH
        YT1=Y1
        XT2=X2
        YT2=Y2
        SCT=SCALE
        X1=0.
        Y1=0.
        X2=XLN
        Y2=-10.
        SCALE=39.37
C/*  COUNTERACT CPTRNS
        WW=WIDTHL
        IF(WW.GT.YNEW2-YNEW1) WW=(YNEW2-YNEW1)*.75
        CALL croshach(NPTS,ABS(ROT),D,0,IDUM,
     +                      LSTYPE,WW,DMARK,ILPEN,ISPEN)
        IF(ROT.LT.0.) CALL croshach(NPTS,ABS(ROT)+90.,D,0,IDUM,
     +                      LSTYPE,WW,DMARK,ILPEN,ISPEN)
        X1=XT1
C/*  RESET MIN/MAX
        Y1=YT1
        X2=XT2
        Y2=YT2
        SCALE=SCT
999    TURN=.TRUE.
       RETURN
       END
