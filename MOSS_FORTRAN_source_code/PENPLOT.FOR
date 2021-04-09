C*****************************************************************************
C
C       P R O G R A M    P E N P L O T
C
C*****************************************************************************
C
        PROGRAM PENPLOT
$include:'dbchan.inc'
$include:'dnames.inc'
C$include:'xypnts.inc'
C$include:'rspnts.inc'
        COMMON /ATPEN/ IANP,NCLOW,NCHI
        COMMON /PLCHN/ ICHAN,X9,X10,ITY
        COMMON /FLS/   IUNIT,IFILE(5)
$include:'whmap.inc'
C        COMMON /WHMAP/ ISTRCT(300),NSTRCT
        COMMON /PLACT/ MACTS
$Include:'mfast.inc'
C        COMMON /MFAST/ IFBUF(259)
$Include:'strng.inc'
C        COMMON /STRNG/ ICARD(80),K
        COMMON /TABLE/ JTABLE(40)
$Include:'vwnd.inc'
C        COMMON /VWND/  XORGIN,YORGIN,XALEN,YALEN,
C     1                 XSTART,YSTART,XLEN,YLEN,IBW
        COMMON /DIMEN/ X1,X2,Y1,Y2,XLN,YLN,SCALE,FACT,X0,Y0
        COMMON /PDASHP/ DASHV
        COMMON /CRNRXY/ CORNX(5),CORNY(5)
        COMMON /CCORNQ/ SB,NB,EB,WB
C$Include:'work.inc'
        COMMON /WORKP/  Item(128),IBUFF(128), MAP(25),
     1                 ICOM(10),JCOM(10),label(80)
$include:'type.inc'
C        COMMON /TYPE/   IDATYP(40),NACTS
        COMMON /LEGAL/  ISHADE,IDASH,ROT,D,MOSIAC,LPEN,LDASH,LSTYPE,
     1                  WIDTHL,DMARK
        COMMON /PENS/   ILPEN,IPPEN,ISPEN,IAPEN
        COMMON /BLMSYMC/  SYMANG,SHITE,SWIDE
        COMMON /FONTHITE/ CHARH,IFONTN,SLANT
        COMMON /PENSTP/   PSTOP,IHOLD(6),ICOLR(6),ITHK(6)
        COMMON /ROTATE/   THETAD,TURN,SCAL
        COMMON /ATTRIBUTE/ IATYP,IATYPE(10),IWORDS(10),ILOCAT(10),
     1                     IATY(10),ILABL
        COMMON /PPLOTA/ ISKIP,ISECTOR
        COMMON /UNIT/   CHANEL,ATRBTE,POLYGON,ADRESS
        COMMON /SELECT/ IMAPS,Idesc(31),IACT(5,11)
C
        CHARACTER*32  CBUF
        CHARACTER*128 CPLOTFILE,TPLOTFILE
        INTEGER IPLOTFILE(25),NAME(25),kcom(25),kfontf(4)
        integer ititlf(4),jsec(2)
        CHARACTER*1 SOL
        character*2 ifontnum
        DOUBLE PRECISION SB,NB,EB,WB
        DIMENSION CORNER(8),ISTEST(30), MBRR(8)
        DIMENSION WIND(4),MCOM(10),IFTYPE(4,4),xbuff(3),jub(3)
        INTEGER CHANEL,ATRBTE,POLYGON,ADRESS
        LOGICAL PSTOP,TURN,SUBJ,FOUND, MCOMP1
        INTEGER map1(2), COMPARE
        EQUIVALENCE (CORNER(1),IBUFF(100)),
     +              (CPLOTFILE,IPLOTFILE),
     +              (CBUF,IBUFF)
C
        data kfontf/2HFO,2HNT,2HS.,2HDT/
        data ititlf/2HTI,2HTL,2HE ,2H  /
        DATA INOS/10/
        DATA IFTYPE/2HPO,2HIN,2HT ,2H  ,
     +              2HLI,2HNE,2H  ,2H  ,
     +              2HPO,2HLY,2HGO,2HN ,
     +              2H??,2H??,2H  ,2H  /
        TURN=.FALSE.
        CALL Initl(ier)
        if(ier.gt.0)stop
C
C  OPEN CHANNELS TO DISK ARRAYS
C
C       OPEN HERSHEY FONTS FILE
C
        nnchar=5
        CALL HSYMB(22,-61,0.,0.,0.,0.,Kfontf,0.,0.,0.,0,nnchar,IER)
        IDEV=2
C*  DEVICE=2=PLOTTER
C
C  GET CORRECT WINDOW
C
      CALL WHWND(X1,Y1,X2,Y2)
       IF (X1 .LT. 1. .OR. Y1 .LT. 1. ) GO TO 895
        LPINCH=10
        ROT=0.0
        D=0.1
C        XINC=XMAX/FLOAT(INOS)
        MACTS=0
C
  1    CONTINUE
C
C  GET INFO ON SPECIFIED ACTIVE DATA
C
  40  CALL DAGET(ICARD,K,JTABLE,40,MACTS)
C
      IF(MACTS.NE.0) GO TO 51
C
C  NO DATA FILES WERE SPECIFIED, REQUEST USER TO SELECT A DATA SET.
C
      WRITE(*,1003)
 1003 FORMAT(/' Enter active map ID.s to be plotted'\)
      CALL READIN(ICARD,lenicd,K,iend)
      K=0
      GO TO 40
C
  51  CONTINUE
C******************************************************************
       write(*,52)
52     format(/,1x,'Output Plot Filename: ',\)
       call readin(map,25,is,ie)
       call openf(16,map,0,0,0,ier)
       if(ier.ne.0)then
        write(*,*)' Problem opening plot file'
        stop
       endif
       ldev=16
21     CONTINUE
C******************************************************************
C
C       CALL THE SCALE ROUTINE (SCL), WHICH WILL ASK THE USER
C       TO ENTER SCALE, AND INDICATE IF A GOOD SCALE HAS BEEN ENTERED.
C
C       ALSO RETURNS X,Y LENGTH TO COMMON DIMENSION
C
C       SCALE=SCL(DUM)
        call devprep(mdev,ipsize,ibplot)
C
        I = 0
        CALL PLOTS(mdev,ipsize,ldev)
        CALL pendv(0.,0.,-3,IDEV)
C
C.....FIND LAT/LONG NORTH AND ALLOW USER TO OVERRIDE ROTATION
C
      IACTIV=JTABLE(1)
      THETAD=0
C      CALL FNDNORT  (THETAD,IACTIV)
c104   WRITE (*,105) THETAD
c105   FORMAT (/' Default rotation for lat/long north alignment = ',F5.3,
c     1 ' degrees,'/
c     1 ,' Enter 0 if you do not want the plot to be rotated,'/
c     1 ,' Enter CR for the default rotation.')
c      WRITE (*,110)
c110   FORMAT (' Enter the angle to rotate the plot in degrees '\)
c      CALL READIN (ICARD,lenicd,K,ie)
c      IF (ICARD(1).NE.ICHAR(' '))
c     +         CALL FNUM (ICARD,80,THETAD,K)
c      IF (THETAD.LT.-360.OR.THETAD.GT.360) GO TO 104
       TURN=.FALSE.
C
      DO 3456 I=1,10
        icom(i)=icard(i)
        IF(ICARD(I).EQ.0) Icom(I)=ICHAR(' ')
3456  CONTINUE
      CALL PACKC(icom,JCOM,10)
        X9=XLN/2.0+1.0*FACT
C x9=represents the logical center of the map area.
        X10=.12*FACT
C******************************************************************
C          TO ALLOW A LARGER PLOT, ALLOW ELIMINATION OF MAP BORDER
C
c        WRITE(*,14001)
c14001 FORMAT( /,' Without border you cannot have bar scale or legend',
c     +/, ' Do you want a border ? [CR=YES] '\)
c      call readin(ibplot,1,is,ie)
        IF(IBPLOT.NE.ICHAR('N')) GO TO 3490
C*  PLOT BORDER
C          NO MAP BORDER
C          PUT MAP SCALE BEFORE PLOT AT 90 DEGREES
        CALL SYMBOL(.25*fact,.25*fact,.14*fact,8HSCALE 1:,90.,8)
        CALL HPNUMBER(999.,999.,.14*fact,SCALE,90.,-1)
        X0=(4.0*FACT)/2.
        Y0=3.0*FACT
        x0=0.0
        y0=0.0
C
        GO TO 17450
C******************************************************************
C
3490    CALL NEWPEN(1)
        X0=(4.0*FACT)/2.
        Y0=3.0*FACT
        CALL ZVERIFY(0.,0.)
c
c  compute margin for border
c
        YACT=(1.19+FLOAT(MACTS+1)*.19)*FACT
        YTEMP=0.0
        IF(YACT.GT.Y0) YTEMP=YACT-Y0
        CALL DRAWB(X0,Y0,YTEMP)
        CALL SYMBOL(X9-.65*FACT,2.64*FACT,X10,9HSCALE 1: ,0.,9)
        CALL HPNUMBER(999.,999.,X10,SCALE,0.,-1)
C       DETERMINE IF THE SCALE BARS ARE DESIRED.
C
        WRITE(*,14100)
14100   FORMAT(/' Do you want a bar scale on this plot ? [CR=YES] '\)
        call readin(ibar,1,is,ie)
        IF(IBAR.EQ.ICHAR('N'))GO TO 17450 
C/*15000
      TH=0.0
      I=1
      ITY=2
      ONE=FACT*SCALE/24000.
      CALL BARCL(X9-2.64*FACT,2.2*FACT,TH,.125*FACT,5.28*FACT,I,ITY)
      CALL HPNUMBER(X9-2.66*FACT,2.4*FACT,.10*FACT,ONE,0.0,1)
      CALL SYMBOL(X9,2.4*FACT,.10*FACT,1H0,0.0,1)
      CALL SYMBOL(999.,999.,.1*FACT,5H MILE,0.,5)
      CALL HPNUMBER(X9+2.62*FACT,2.4*FACT,.10*FACT,ONE,0.0,1)
      I=2
      ITY=1
      CALL BARCL(X9-1.64*FACT,1.8*FACT,TH,.125*FACT,3.2808*FACT,I,ITY)
      CALL HPNUMBER(X9-1.65*FACT,1.95*FACT,.1*FACT,ONE,0.0,1)
      CALL SYMBOL(X9,1.95*FACT,0.1*FACT,1H0,0.0,1)
      CALL HPNUMBER(X9+1.64*FACT,1.95*FACT,.1*FACT,ONE,0.0,1)
      CALL SYMBOL(999.,999.,.1*FACT,10H KILOMETER,0.,10)
15000 CONTINUE
C
C     PLOT ROTATED NORTH ARROW
C
      TURN=.TRUE.
      CALL pendv(X9-3.50*FACT,1.3*FACT,3,IDEV)
      CALL pendv(X9-3.50*FACT,2.32*FACT,2,IDEV)
      CALL pendv(X9-3.40*FACT,2.2*FACT,2,IDEV)
      CALL SYMBOL(X9-3.50*FACT,2.37*FACT,X10,2HN ,0.,1)
      TURN=.FALSE.
C
C       DETERMINE IF THE USER WANTS THE LABEL INFO PLOTTED
C
c17450   WRITE(*,16800)
c16800   FORMAT(' Do you want a TITLE on the plot ? [CR=YES] '\)
c        call readin(ilab,1,is,ie)
c        IF(ILAB.EQ.ICHAR('N'))GO TO 18000
17400   CONTINUE
C***********************************************************************
C          THIS IS THE TITLE ON THE PLOT
C***********************************************************************
C        call OPENf(19,ititlf,1,0,0,IER)
C        IF ( IER.NE.0 ) GO TO 18000
C        READ(19,*,ERR=17999,iostat=ier)IFONT,DX,DY
C        READ(19,'(a32)',ERR=17999,iostat=ier)CBUF
17450   write(*,*)
        write(*,1717)
1717    format(' Title for this PLOT (end w/ @)',/,' ?',\)
        read(*,'(a32)')cbuf
        ilab=ichar('Y')
        if(cbuf(1:1).eq.' ') then
          ilab=ichar('N')
          go to 18000
        endif
        nch=index(cbuf,char(64))
        NCH=NCH-1
        IF(NCH.LE.0) then
         do 9092 ll=32,1,-1
9092     if(cbuf(ll:ll).gt.' ')go to 9093
9093    nch=ll
        endif
        if(nch.le.0)go to 18000
        write(*,*)
        write(*,9090)
 9090   format(' Font Number (1-8,11-16,23-24,26,32,33,51-53) ',\)
        read(*,'(a2)')ifontnum
        if(ifontnum(1:1).eq.' ')then
         ifont=1
        else
         read(ifontnum,'(i2)')ifont
        endif
        HH=.5*fact
        XTIT=X9 - HH*NCH/2.
        dy=.5
        ytit=(yln+(.25*fact))+y0
        CALL HSYMB(IDEV,IFONT,XTIT,ytit,HH,DY*FACT,IBUFF,
     +             0.,0.,0.,0,NCH,IER)
C17999   CLOSE(19)
C******************************************************************
18000   CONTINUE
C       CONVERT WINDOW TO INCHES.
C
        CALL CPTRNS(X1,XI1,X1,SCALE)
        CALL CPTRNS(Y1,YI1,Y1,SCALE)
        CALL CPTRNS(X2,XI2,X1,SCALE)
        CALL CPTRNS(Y2,YI2,Y1,SCALE)
C
        CALL pendv(X0,Y0,-3,IDEV)
C        TURN=.TRUE.
C
C  PLOT TICS
C
        WRITE(*,5049)
5049    FORMAT(' Do you want TIC marks shown on plot ? [CR=NO] '\)
        call readin(icard1,1,is,ie)
        IF(ICARD1.EQ.ICHAR('Y')) 
     +    CALL TICS(X1,Y1,X2,Y2)
C        IF ( MCOMP1(ICARD,'Y',1,IER) )
C     +       CALL TICS(X1,Y1,X2,Y2)
C
        WRITE(*,5051)
5051    FORMAT(' Do you want mapname(s) and selection criteria',
     +  ' displayed ? [CR=NO] '\)
        call readin(imaps,1,is,ie)
C
        KTEST=0
        DO 5050 I=1,MACTS
        ISTEST(I)=0
 5050   CONTINUE
CPRIME
        CALL OPENF(icdcdt,idesdt,1,62,2,IER)
C
C       Loop through each data set selected(MACTS= # of sets)
C
       DO 50 I=1,MACTS
C
C  SET DENSITY FOR POSSIBLE POLYGON SHADING
C
        PINCH=LPINCH
        D=XINC/PINCH
        LPINCH=LPINCH+3
C
        CALL GTMAP(ICOM,JTABLE(I))
        JENDEX=(JTABLE(I)/7)+1
        IDTYPE=IDATYP(JENDEX)   
C/*DATA TYPE (1=SYMBOL, 2=LINE, 3=POLY)
C
 550    ICHAN=15
        CALL OPENF(ICHAN,ICOM,1,256,2,IER)
        IF ( IER.NE.0 ) GO TO 902
C
C       READ HEADER TO DETERMINE IF THIS IS A MULTIPLE ATT FILE
C
        CALL RDBLK(ICHAN,1,IBUFF,128,IER)
C
        ISHADE=0
        LSTYPE=1
        IDASH=0
        LDASH=0
        K=0
        NKARS=0
C*******************************************************
C       GET SELECTION CRITERIA
C
        read(icdcdt,rec=jendex,err=886,iostat=ier)idesc
C        CALL RDBLK(ichan,JENDEX,ISUB,16,IER)
        IF (Idesc(31) .LT. 1  .OR.  Idesc(31) .GT. 10) Idesc(31)=11
 886    DO 887 II=1,10  
C/*  BLANK OUT MAP NAME
        IF(ICOM(II).LE.ICHAR(' ')) icom(ii)=ichar(' ')
887    CONTINUE
        JDTYPE=IDTYPE
        IF(IDTYPE.LE.0 .OR. IDTYPE.GT.3) JDTYPE=4
        
        if(idesc(31).ne.3)then
        WRITE(*,890) JENDEX,(ICOM(II),II=1,5),
     +                   (IFTYPE(II,JDTYPE),II=1,4),
     +                   (IACT(KL,idesc(31)),KL=1,5),(Idesc(KL),KL=1,15)
 890    FORMAT(/,' Map #',I5,' = ',5A2,4X,4A2,'data',/,
     +           ' Selection Criteria = ',5A2,2X,15A2,/)
        else
         do 8912 ll=1,30
8912     if(idesc(ll).ne.0)kj=ll      
         write(*,8911)jendex,(icom(ii),ii=1,5),
     + (iftype(ii,jdtype),ii=1,4),
     + (iact(kl,idesc(31)),kl=1,5),(idesc(kl),kl=1,kj)
8911   format(/,' Map #',i5,' = ',5a2,4x,4a2,' data',/,
     + ' Selection Criteria = ',5a2,2x,6i5,/,16i5,/,8i5)
       endif
C
C       IF THIS IS A MULT ATT FILE - PROMPT FOR LABEL INFO
C
C        
        IF(IBUFF(76).NE.1) GO TO 879
        WRITE(*,871)
871     FORMAT(' Do you want multiple attribute labeling [CR=NO] ? '\)
        call readin(imult,1,is,ie)
        IF(IMULT.NE.ICHAR('Y')) GO TO 879
        SUBJ=.TRUE.
        ILABL=0
        CALL GTNAM(JTABLE(I),4,kcom,MAP,IER)
        ICHAN2=26
        CALL OPENF(ICHAN2,MAP,1,256,2,IER)
        CHANEL=ICHAN2
        IF(IER.NE.0) GO TO 903
        CALL INITIAL
        WRITE(*,870)
870     FORMAT(/,' Enter the attributes in the order you want them',
     1           ' plotted (BOTTOM to TOP) '\)
872     CONTINUE
          ILABL=ILABL+1
          IF(ILABL.LE.10) GO TO 874
          WRITE(*,875)
875       FORMAT(/,' The maximum number of labels per stack is 10.',/,
     1             ' Continuing program execution.')
          GO TO 876
874       WRITE(*,873) ILABL
873       FORMAT(/,' For Label ',I3)
          CALL RETRVL(FOUND,IBUFF,SUBJ)
          IF(.NOT.FOUND) GO TO 876
          IATY(ILABL)=IATYP
          IF(IATYP.EQ.0) GO TO 872
          IATYPE(ILABL)=IBUFF(84)
          IWORDS(ILABL)=IBUFF(85)
          ILOCAT(ILABL)=IBUFF(86)
          GO TO 872
876     CONTINUE
        ILABL=ILABL-1
        IF(ILABL.NE.0) WRITE(*,881)
881     FORMAT(' Do you want to skip blank labels ? [CR=YES] '\)
        call readin(iskip,1,is,ie)
        GO TO 891
878     CONTINUE
        ILABL=1
        IATY(ILABL)=-1
C
C  ASK USER WHETHER THEY WANT SUBSTRINGS OF LABELS SHOWN ON MAP
C
 891    WRITE(*,25400)
25400   FORMAT(' [Low,High] char positions of label to plot ',
     +   '[CR=none,-1=item#] '\)
        CALL READIN(ICARD,lenicd,K,ie)
        CALL INUM(ICARD,NCLOW,K)
        IF(NCLOW.EQ.0) GO TO 879    
C/*  NO SUBJECT LABELS
        IF(NCLOW.LT.-1 .OR. NCLOW.GT.30) GO TO 891  
C/*  BAD INPUT
        IF(NCLOW.EQ.-1) GO TO 892      
C/*  USE ITEM #
        CALL INUM(ICARD,NCHI,K)
        IF(NCHI.GT.30) NCHI=30
        IF(NCHI.LT.0 .OR. NCLOW.GT.NCHI) GO TO 891  
C/*  BAD INPUT
 892    WRITE(*,893)
 893    FORMAT(' Enter label HEIGHT and FONT # to use [CR=.07,12] '\)
        CALL READIN(ICARD,lenicd,K,ie)
        CALL FNUM(ICARD,80,CHARH,K) 
C/*  GET SYMBOL HEIGHT
        IF(CHARH.LE.0.) CHARH=.07   
C/*  SET DEFAULT
        IF (K.GE.81) K=0
        CALL INUM(ICARD,IFONTN,K)
C/*  HERSHEY FONT #
        IF(IFONTN.LE.0 .OR. IFONTN.GT.52) IFONTN=12  
C/*  DEFAULT
 880    WRITE(*,894)
 894    FORMAT(' Enter degree of slant for labels ',
     +         ' [CR=0,RANGE=-90 to 90]'\)
        CALL READIN(ICARD,lenicd,K,ie)
        CALL FNUM(ICARD,80,SLANT,K)
        IF(SLANT.LT.-90..OR.SLANT.GT.90.) GO TO 880
 882    WRITE(*,883)
 883   FORMAT(' Enter alignment code for labels - NW,NE,SW,SE [CR=NE]'\)
        call readin(jsec,2,is,ie)
        isector=2hNE 
        if(jsec(1).eq.ichar('N')) then
           if(jsec(2).eq.ichar('E'))isector=2hNE
           if(jsec(2).eq.ichar('W'))isector=2hNW
        else
           if(jsec(2).eq.ichar('E'))isector=2hSE
           if(jsec(2).eq.ichar('W'))isector=2hSW
        endif
        if(isector.eq.ichar('N'))go to 882
C***********************************************************************
        CALL PENASS(10HLabels    ,IAPEN)
C
C       DETERMINE WHAT PEN IS TO BE USED TO DRAW THE DATA
C
879     IF(IDTYPE.EQ.1) CALL PENASS(10HSYMBOLS   ,ISPEN)
        IF(IDTYPE.EQ.2) CALL PENASS(10HLINES     ,ILPEN)
        IF(IDTYPE.EQ.3) CALL PENASS(10HPOLYGONS  ,IPPEN)
C
C  FOR POLYGON DATA, ASK THE USER IF THEY WISH TO SHADE IT
C
41      IF(IDTYPE.NE.3) GO TO 43
        WRITE(*,1006)
 1006   FORMAT(' Do you want POLYGONS shaded ? [CR=YES] '\)
        call readin(ishade,1,is,ie)
        IF(ISHADE.EQ.ICHAR('N'))GO TO 43
        CALL PENASS(10HSHADING   ,ILPEN)
        ishade=ichar('Y') 
C
250     CONTINUE
        WRITE(*,28000)
28000 FORMAT(' Enter SHADE angle (0-360 degrees) [-ANGLE=crosshatch] '\)
        rot = 0.0
        READ(*,*) ROT
        IF(ABS(ROT).LE.360.)GO TO 260
        WRITE(*,25700) ROT
25700   FORMAT(' Angle must be between -360 and 360 degrees'/
     1  '        (you entered ',F7.2,' degrees). Try again.')
        GO TO 250
260     CONTINUE
        WRITE(*,28800)
28800   FORMAT(' Enter DISTANCE between SHADE lines (inches) [CR=.1] '\)
        CALL READIN(ICARD,lenicd,K,ie)
        CALL FNUM(ICARD,80,D,K)
        IF(D.LE.0.0 .OR. D.GT.0.5) D=.1
        WRITE(*,1056)
 1056   FORMAT(' Enter LINE TYPE for SHADE lines [CR=5=solid] '\)
        CALL READIN(ICARD,lenicd,K,ie)
        CALL INUM(ICARD,LSTYPE,K)
        IF(LSTYPE.LE.0) LSTYPE=5
        GO TO 645
 43     CONTINUE
        IF(IDTYPE.NE.1) GO TO 799
C
C          ***** SYMBOL DATA *****
        LSTYPE=-1
        WRITE(*,701)
 701    FORMAT(' Do you wish to assign SYMBOL type ? [CR=YES] '\)
        call readin(icard1,1,is,ie)
        IF(ICARD1.EQ.ICHAR('N')) GO TO 650 
C/*  NO -- DEFAULT = 'X'
 710    WRITE(*,702)
 702    FORMAT(' Enter SYMBOL code [0 or 99 = none] '\)
        call readin(lstype,1,is,ie)
        IF(LSTYPE.GE.0) GO TO 720
 715    WRITE(*,*) ' Bad SYMBOL code, try again'
        GO TO 710
 720    WRITE(*,721)
 721    FORMAT(' Enter SYMBOL height, width, angle '\)
        CALL READIN(ICARD,lenicd,K,ie)
        CALL FNUM(ICARD,80,SHITE,K)
        CALL FNUM(ICARD,80,SWIDE,K)
        CALL FNUM(ICARD,80,SYMANG,K)
        GO TO 650
 799    IF(IDTYPE.NE.2) GO TO 650
C
C  ******  LINE DATA *******
C
        LSTYPE=5
        WRITE(*,639)
 639    FORMAT(' Assign LINE type ? [CR=YES] '\)
        call readin(icard1,1,is,ie)
        IF(ICARD1.EQ.ICHAR('N')) GO TO 650
 640    WRITE(*,641)
 641    FORMAT(' Enter LINE code [0=invisible, -1=contour] '\)
        call readin(icard,80,is,ie)
        call inum(icard,lstype,is) 
        IF(IMULT.NE.ICHAR('Y').OR.LSTYPE.NE.-1) GO TO 644
        WRITE(*,646)
 646   FORMAT(/,' Contouring not allowed when multi-attribute labeling',
     1         ' is in effect. ',/)
        GO TO 640
 644    IF(LSTYPE.GE.-1) GO TO 645
 642    WRITE(*,643)
 643    FORMAT(' Bad LINE type choice, try again')
        GO TO 640
 645    CALL DLINDEF(LSTYPE,ISPEN,WIDTHL,DMARK)
  650   CONTINUE
C
        IF(IBW.EQ.0) XMAX=MAX(XALEN,YALEN)
        IF(IBW.EQ.1) XMAX=MAX(XLEN,YLEN)
        JINX=I
        DO 55 JK=1,MACTS
        IF(JTABLE(I).EQ.ISTEST(JK))GO TO 56
  55    CONTINUE
        KTEST=KTEST+1
        ISTEST(KTEST)=JTABLE(I)
        GO TO 57
  56    CONTINUE
        JINX=-I
  57    CONTINUE
        IF(IBPLOT.EQ.ICHAR('N')) GO TO 58
C
C  PROGRAMMABLE PEN STOPS
C
        WRITE (*,7777)
7777    FORMAT(' Do you want a pen stop ? [CR=NO] '\)
        call readin(icard1,1,is,ie)
        PSTOP=.FALSE.
        IF(ICARD1.EQ.ICHAR('Y').or.icard1.eq.ichar('y')) PSTOP=.TRUE.
        IF(.NOT.PSTOP) GO TO 7782
        WRITE (*,7778)
7778    FORMAT(' Enter pen holder(s) to change '\)
        CALL READIN(ICARD,lenicd,K,ie)
        CALL PACKC(ICARD,IHOLD,12)
        WRITE (*,7779)
7779    FORMAT(' Enter color(s) '\)
        CALL READIN(ICARD,lenicd,K,ie)
        CALL PACKC(ICARD,ICOLR,12)
        WRITE (*,7780)
7780    FORMAT(' Enter thickness(es) '\)
        CALL READIN(ICARD,lenicd,K,ie)
        CALL PACKC(ICARD,ITHK,12)
C
C  PLOT DESCRIPTORS
C
7782    CONTINUE
        XP1=X0-4.0*FACT
        XP2=72.0
        YP1=Y0-6.0*FACT
        YP2=32.0
        CALL PLOTH(ICHAN,ICOM,JINX,IDTYPE)
58      IAA=0
C
C  GET POINTER INFO FOR DATA IN MAPPTS
C
54     CALL APGET(JTABLE(I),IPOT1,ISTOP,NTOTAL)
      IF(NTOTAL.EQ.0) GO TO 35
C
        CALL SETW(XI1,YI1,XI2,YI2) 
C/*  SET SCISSOR PLOT WINDOW
      DO 49 J=IPOT1,ISTOP
C
C  GET DATA POINTER
C
      CALL GETFAST(J,MBRR,JNDEX,ISUBJ,IATR,IEX1,IER)
      IF(IER.NE.0) GO TO 49
      CALL GETWND(JNDEX,WIND,ICHAN)
C
C  DOES DATA FALL IN THE WINDOW?
C
       IF(INVWND(WIND(1),WIND(2),WIND(3),WIND(4),
     1 X1,X2,Y1,Y2).EQ.0) GO TO 49
C
C  GET DATA
C
       CALL RDBLK(ICHAN,JNDEX,ITEM,128,IER)
       DO 47 JJ=1,128
         IBUFF(JJ)=ITEM(JJ)
47     CONTINUE
C       TURN=.TRUE.
       CALL PLOTP(ICHAN,JNDEX,IBUFF,WIND(1),WIND(3),ICHAN2)
  49  CONTINUE
       CALL SETW(0.,0.,0.,0.)
C/*  TURN OFF PLOT WINDOW
       CLOSE(ICHAN)
       CLOSE(ICHAN2)
       CALL NEWPEN(LPEN)
  50  CONTINUE
C      CLOSE(17)
C          PLOT ANY DATA CREATED BY ANNOTATE COMMAND
C
        TURN=.FALSE.
 59     WRITE(*,101)
 101    FORMAT(/,' ANNOTATE file name [CR=none; ',
     +           ' ORIGIN=return to origin] '\)
        CBUF = ' '
        call readin(map,25,is,ie)
        NCH = (ie-is)+1
        if(ie.eq.0)go to 70
C/*  NO MORE ANNOTATE FILES
C          "ORIGIN" MEANS GO BACK TO ORIGIN AND STOP
        if(map(1).eq.ichar('O').and.map(2).eq.ichar('R').
     + and. map(3).eq.ichar('I'))go to 177
        CALL OPENF(21,map,1,0,0,IER)
        IF ( IER.NE.0 ) GO TO 68
        CALL SETW(XI1,YI1,XI2,YI2) 
C/*  SET SCISSOR PLOT WINDOW
 60   READ(21,61,ERR=67,END=67)(ITEM(I),I=1,40)
 61     FORMAT(40A2)
      READ(21,62,ERR=69,END=69)XP,YP,ICOLOR,HITE,ANGLE,IFONT,WIDTH
 62     FORMAT(2F10.0,I5,2F10.0,I5,F10.3)
        IF(IFONT.EQ.0) IFONT=16
        IF(ABS(WIDTH).LT..00001) WIDTH=HITE
        DO 65 NCH=80,1,-1
        IF(ITEM(NCH).NE.ICHAR(' ')) GO TO 66
 65     CONTINUE
        GO TO 60
 66     CALL CPTRNS(XP,XT,X1,SCALE)
        CALL CPTRNS(YP,YT,Y1,SCALE)
        CALL NEWPEN(ICOLOR)
        CALL HSYMB(IDEV,IFONT,XT,YT,WIDTH,HITE,ITEM,
     1             0.,0.,ANGLE,0,NCH,IER)
        GO TO 60
   67   CLOSE(21)
        GO TO 59
 68     WRITE(*,*) 'ANNOTATE file does not exist'
        GO TO 59
 69     WRITE(*,*) 'Error READing ANNOTATE file - ',
     +                  'Bad record ignored'
        WRITE(*,169) (ITEM(I),I=1,30)
 169    FORMAT('BAD AFTER TEXT=',30A2)
        GO TO 60
C******************************************************************
C
C       ALL DATA SETS HAVE BEEN PROCESSED.
C       CLOSE UP PLOT
C
70      CONTINUE
        CALL SETW(0.,0.,0.,0.) 
C/*  TURN OFF PLOT WINDOW
        CALL pendv(-X0,-Y0,-3,IDEV)
        CALL ZVERIFY(0.,0.)
        CALL pendv(XLN+5.0*FACT,0.0,-3,IDEV)
        CALL pendv(0.,0.,999,IDEV)
      GO TO 998
177     CONTINUE       
C/*  GO BACK TO ORIGIN
        CALL SETW(0.,0.,0.,0.)  
C/*  TURN OFF PLOT WINDOW
        CALL pendv(-X0,-Y0,-3,IDEV)
        CALL ZVERIFY(0.,0.)
        CALL pendv(0.,0.,999,IDEV)
      GO TO 998
35    CONTINUE
      WRITE(*,1000)
 1000 FORMAT(2X,' THEME selected doesnt exist',/
     1  'do you want to try again?'/'___')
      GO TO 99
 895    CONTINUE
       WRITE(*,37500)
37500   FORMAT(/' You must WINDOW at least one of the themes.')
       GO TO 99
 901    CONTINUE
        WRITE(*,37700)
37700   FORMAT(/' .ERROR on opening DESCRIBE.FA in PENPLOT'/)
        GO TO 99
902     CONTINUE
        WRITE(*,38100)
38100   FORMAT(/' .ERROR on opening MAP file in PENPLOT'/)
        GO TO 99
903     CONTINUE
        WRITE(*,38300) MAP
38300   FORMAT(/' .ERROR opening .AT file = ',15A2,/)
        GO TO 99
 904    CONTINUE
        WRITE (*,38400)
38400   FORMAT(/' .ERROR on opening DISK arrays in PENPLOT'/)
 99   CONTINUE
 998    K=0
        close(ldev)
       END
