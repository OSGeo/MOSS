C **********************************************************
C
C           P R O G R A M   L P O V E R
C
C FUNCTION:  THIS IS THE LOGICAL DRIVER FOR THE POLYGON AND POINT/LINE
C            INTERSECTION UNION ROUTINE.
C *******************************************************
C
$Include:'dbchan.inc'
$Include:'dnames.inc'
        COMMON /ATTFET/ KAT(86),LBAT
        COMMON /ITEMMS/ ITEM1,ITEM2
        COMMON /IO/     NPRNT,IOIN
        COMMON /PNTFET/ NPOINT
        COMMON /WHMAP/  ISTRCT(300),NSTRCT
$Include:'strng.inc'
        COMMON /TABLE/  ITABLE(40)
        COMMON /O/      PMAX,PMIN,QMAX,QMIN,XMAX,XMIN,YMIN,YMAX
        COMMON /DOT/    NSLAND,JH,IDS(350)
        COMMON /OFFST/  SXMIN,SYMIN
        COMMON /OSAVE/  ISUB(15),NPOLYS,LTYPE2,IDISP,NOSAVE,ICTSUB
        COMMON /WORK/   IBUFF(128),MAP(25),ICOM(10),JCOM(10)
$Include:'xypnts.inc'
$Include:'pqpnts.inc'
$Include:'rspnts.inc'
        COMMON /STATY/  NOINT,IDD2
        COMMON /TYPE/   IDATYP(40),NACTS
$Include:'mfast.inc'
C
       DIMENSION KTEMP(128),IHEAD(256),NAME1(10),NAME2(10),mapname(25)
        DIMENSION LABL1(15),LABL2(15),mbr(8),map1(25),isubr(30)
        integer mapout(25),mapin1(25),mapin2(25)
        REAL IX1,IY1,IX2,IY2,IWIND1(4),IWIND2(4)
        LOGICAL ATTRI,NOSAVE,SAVIT
        EQUIVALENCE (X1MIN,IBUFF(15)),(X1MAX,IBUFF(17))
        EQUIVALENCE (Y1MIN,IBUFF(19)),(Y1MAX,IBUFF(21))
C
C  SET SOME VARIABLES
C
        KOUNT=0
        KNT = 0
        IPROMT=0
        LINES=1
        IDISP=0
        NPOLYS=0
        NOSAVE=.FALSE.
        ATTRI=.FALSE.
C        IFBUFF(259) = 20
C
C  LOAD COMMON
C
        CALL initl(ier)
        if(ier.ne.0)stop
        NOINT=NPOINT
C
C  GET CURRRENT DISPLAY WINDOW
C
        CALL WHWND(IX1,IY1,IX2,IY2)
        SXMIN=IX1
        SYMIN=IY1
C
C  CHECK FOR ZERO WINDOW
C
        IF(IX2-IX1.EQ.0.0.OR.IY2-IY1.EQ.0.0) GO TO 994
C
        ICHAN3=9
        ICHAN1=10
        ICHAN2=12
        ICTSUB=51
        CALL OPENF(icdcfa,idesfa,1,512,2,IER)
        IF(IER.NE.0) GO TO 911
C
         map(1)=2hza
         call openf(ictsub,map,3,256,2,ier)
C
C  GET ACTIVE DATA SETS
C
        CALL DAGET(ICARD,icardp,ITABLE,40,MACTS)
        IF(MACTS.NE.0) GO TO 10
C
C  PROMPT USER FOR ACTIVE DATA SETS
C
        WRITE(NPRNT,1000)
 1000   FORMAT(' ENTER TWO ACTIVE IDs.(POLYGON FIRST)(CR=QUIT)'\)
        CALL READIN(ICARD,lenicd,icardp,istop)
        IF(ICARD(1) .EQ. 0) GOTO 999
        CALL DAGET(ICARD,icardp,ITABLE,40,MACTS)
        LINES=LINES+3
C
C  GET POINTER SET AND NUMBER OF FEATURES IN EACH
C
  10    CONTINUE
        INDEX1=ITABLE(1)
        INDEX2=ITABLE(2)
        CALL APGET(INDEX1,ISTAR1,ISTOP1,NTOT1)
        CALL APGET(INDEX2,ISTAR2,ISTOP2,NTOT2)
C
C  CHECK FOR CORRECT DATA TYPE
C
        IFTYP1=IDATYP(INDEX1/7+1)
        IFTYP2=IDATYP(INDEX2/7+1)
        IF(MOD(IFTYP1,10).NE.3) GO TO 915
        IF(MOD(IFTYP2,10).LT.1  .OR.  MOD(IFTYP2,10).GT.2) GO TO 915
C
C  GET NAME OF SOURCE MAPS
C
        CALL GTMAP(NAME1,INDEX1)
        CALL GTMAP(NAME2,INDEX2)
C
C  OPEN INPUT FILES
C
        call adddir(name1,mapin1)
        CALL OPENF(ICHAN1,mapin1,1,256,2,IER)
        IF(IER.NE.0) GO TO 989
        call adddir(name2,mapin2)
        CALL OPENF(ICHAN2,mapin2,1,256,2,IER)
        IF(IER.NE.0) GO TO 914
C
C  READ IN HEADER BLOCK FROM A MAP AND WRITE IT TO TEMP SAVE FILE
C
        CALL RDBLK(ICHAN1,1,IHEAD,1,IER)
        CALL RDBLK(ICHAN1,2,IHEAD(129),1,IER)
        CALL RDBLK(ICHAN2,1,KTEMP,1,IER)
        IF(KTEMP(76).EQ.1) ATTRI=.TRUE.
        IHEAD(65)=IHEAD(77)
C
C  DON'T ASK ABOUT A NEW MAP IF MAKING AN ACTIVE ID
C
        IF (ATTRI) GO TO 51
C
C  GET NEW MAP NAME
C
  12    CONTINUE
        CALL WHTCAL(icom,ier)
        IF (IER .EQ. -99) GOTO 999
C
C  ASK ABOUT SUBJECT MERGE
C
        CALL ASKSUB(IPROMT,MERLAB,MLPNT,NCHARS,IREV,IER)
C
C  OPEN TEMP CHANNEL TO WRITE EVERYTHING TO
C
  51    CONTINUE
        map1(1)=2hg
        CALL OPENF(18,map1,3,0,1,IER)
        IF(IER.NE.0) GO TO 909
C
C  WRITE MAP NAME TO TEMP FILE
C
        CALL PACKC(icom,mapout,10)
        WRITE (18) mapout
C
C  WRITE MAP NAMES TO SAVE TEMP FILE
C
        WRITE (18) mapin1
        write(18)  mapin2
        write(18)(IHEAD(l),l=1,65)
C
C  WRITE OUT DATA TYPE OF RESULTING DATA
C
        write(18)IFTYP2
C
C  NOW ENTER MAIN COMPUTATION LOOP
C

        write(*,*)
        write(*,*)' Executing - Please wait  -- '
        DO 200 II=ISTAR1,ISTOP1
C
C  GET POINTER FOR THIS ITEM
C
          CALL GETFAST(II,mbr,IDD1,isubj,iattr,ixtra,IER)
          IF(IER.ne.0) GO TO 200
C
C  READ POLYGON FEATURE
C
          CALL RDBLK(ICHAN1,IDD1,IBUFF,1,IER)
          IWIND1(1)=X1MIN
          IWIND1(2)=X1MAX
          IWIND1(3)=Y1MIN
          IWIND1(4)=Y1MAX
C
C  SEE IF IT IS IN THE WINDOW
          IF(INVWND(IWIND1(1),IWIND1(2),IWIND1(3),IWIND1(4),
     1     IX1,IX2,IY1,IY2).EQ.0) GO TO 200
          XMIN=IWIND1(1)-SXMIN
          XMAX=IWIND1(2)-SXMIN
          YMIN=IWIND1(3)-SYMIN
          YMAX=IWIND1(4)-SYMIN
C
C  GET ITEM NUMBER AND POINTER TO SUBJECT
C
          ITEM1=IBUFF(5)
          IPOINT = ibuff(24)
C
C  GET ISLANDS
C
          CALL IGETIS(IBUFF,IDS,NSLAND,ICHAN1,IDD1)
C
C  GET LABEL
C
          CALL GETSUB(ICHAN1,IPOINT,IBUFF,LABL1,ITS)
C
C  GET COORDINATES
C
          CALL RDBLK(ICHAN1,IDD1,IBUFF,1,IER)
          XM=IWIND1(1)-SXMIN
          YM=IWIND1(3)-SYMIN
          CALL CORDXY(IBUFF,NPTS1,IDD1,XM,YM,ICHAN1)
C
C  SET UP POINTER ARRAY FOR POLYGON
C
          IDS(NSLAND+1)=NPTS1+1
C
C  NOW ENTER THE INNER COMPARISON LOOP
C
          DO 100 JJ=ISTAR2,ISTOP2
C
C GET DATA BASE POINTER TO THIS ITEM
C
            CALL GETFAST(JJ,mbr,IDD2,isubj,iattr,ixrea,IER)
            IF(IER.ne.0) GO TO 100
C
C  MBRS OVERLAY ?
C
            CALL GETWND(IDD2,IWIND2,ICHAN2)
            IF(INVWND(IWIND2(1),IWIND2(2),IWIND2(3),IWIND2(4),
     1      IWIND1(1),IWIND1(2),IWIND1(3),IWIND1(4)).EQ.0) GO TO 100
C
C  A POSSIBLE OVERLAP...GET ITEM NUMBER, TYPE, AND LABEL INFORMATION
C
            ITEM2=IBUFF(5)
            LTYPE2=IBUFF(6)
            IPOINT = IBUFF(24)
            CALL GETSUB(ICHAN2,IPOINT,IBUFF,LABL2,ITS)
C
C  GET POINT DATA FOR MINOR POLYGON
C
            CALL RDBLK(ICHAN2,IDD2,IBUFF,1,IER)
            IPTREC = IDD2
            XM=IWIND2(1)-SXMIN
            YM=IWIND2(3)-SYMIN
            CALL CORDPQ(IBUFF,NPTS2,IDD2,XM,YM,ICHAN2)
C
C   INSERT POINTER TO END OF LINE COORDS
C
            JH=0
C
C  PUT WINDOW OF MINOR POLYGON IN COMMON
C
            PMIN=IWIND2(1)-SXMIN
            PMAX=IWIND2(2)-SXMIN
            QMIN=IWIND2(3)-SYMIN
            QMAX=IWIND2(4)-SYMIN
C
C  STORE THE NEW SUBJECT
C
            IF (MERLAB.EQ.1)
     1         CALL NEWSUB(MLPNT,NCHARS,LABL1,LABL2,ISUB)
            IF(MERLAB.NE.1)
     1         CALL MERSUB(LABL1,LABL2,ISUB)
C
C  INTERSECT THE POLYGON WITH THE POINT OR LINE
C
            IF(IFTYP2.EQ.1.OR.IFTYP2.EQ.11) CALL PTBOOLE(NPTS1,IND)
            IF(IFTYP2.EQ.2.OR.IFTYP2.EQ.12) CALL LBOOLE(NPTS1,NPTS2,IND)
C
C  PUT ENTRY IN POINTER FILES IF HAS MULTIPLE ATTRIBUTES
C
            IF(.NOT. ATTRI .OR. IND .NE. 1) GO TO 100
                CALL RDBLK(ICHAN2,IPTREC,IBUFF,1,IER)
                 call putfast(npoint,ibuff(15),iptrec,ibuff(24),
     +                        iptrec,iptrec,ier)
C                CALL UPTDT(IPTREC,NPOINT,KOUNT,IER)
                IF (IER .EQ. -80) GO TO 999
                KNT = KNT + 1
 100      CONTINUE
 200    CONTINUE
C
C  ANY NEW POLYGONS? IF NOT, PRINT MESSAGE AND RETURN
C
        IF(NPOLYS.LT.1) GO TO 997
C
C  SAVE MAP
C
        IF(ATTRI) GO TO 998
C
C   save map
C
        ichan31=31
        call openf(ichan31,mapout,0,256,2,ier)
        call savwrk(ictsub,npolys,18,ichan31)

C        CALL OPENF(icwork,iwork,1,512,2,IER)
C        IF(IER.NE.1) GO TO 912
C        CALL PACKC(MAP,ICOM,10)
        CALL RDBLK(ICHAN31,1,IHEAD,1,IER)
        CALL WRBLK(ICHAN31,2,IHEAD(129),1,IER)
        CALL TUPDIR(ITP1,ITP2,ITP3,icom,IHEAD,iwork,icwork,IER)
        close(ichan31,iostat=ier)
C
C  PRINT OUT PROCESSING STATISTICS
C
        WRITE(NPRNT,1004) NPOLYS
 1004   FORMAT(24H INTERSECTED FEATURES =   ,I5)
        LINES=LINES+2
        GO TO 999
C
C  POINT DATA WITH ASSOCIATED ATTRIBTES...PUT IN ACTIVE TABLE
C
 998    CONTINUE
        CALL FILLC(isubr,30,(2H  ) )
        DO 777 I=1,5
          isubr(I)=NAME1(I)
          isubr(I+5)=NAME2(I)
 777    CONTINUE
         LBAT=(NSTRCT/7)*2+1
        KAT(LBAT) = 4
        KAT(LBAT+1)=0
        I = 10
        NOINT = NOINT + 1
        CALL ADSTAD(IER,NAME2,KNT,NOINT,isubr,I)
        NACTS=NACTS+1
        IDATYP(NACTS)=IFTYP2
        WRITE(NPRNT,779) KNT,NACTS
 779    FORMAT(1X,I5,' ITEMS FOR ACTIVE ID',I3)
        LINES=LINES+2
        GO TO 999
C
C  ERROR RETURNS
C
 997    WRITE(NPRNT,1051)
 1051   FORMAT(' NO POLYGONS INTERSECT')
        LINES=LINES+2
        GO TO 999
 994    write(*,*)' Display window not set.'
        GO TO 999
 989    write(*,*)' Error on accessing input map'
        GO TO 999
 908    WRITE(NPRNT,3088)
 3088   FORMAT(' *LPOVER* ERROR CREATING TEMP FILE')
 909    WRITE(NPRNT,3090)
 3090   FORMAT(' *LPOVER* COULD NOT OPEN TEMP FILE')
        GO TO 999
 911    write(*,*)' Could not open DESCRIBE.FA'
        GO TO 999
 914    write(*,*)' Error on processing second map'
        GO TO 999
 915    WRITE(NPRNT,3015)
 3015 FORMAT(' FIRST MAP MUST BE POLYGON AND SECOND MAP POINT OR LINE')
        LINES=LINES+2
        GO TO 999
C
C  ALL DONE
C
 999    CONTINUE
        CLOSE(ICHAN1,iostat=IER)
        close(ichan2,iostat=ier)
        CALL PUTFAST(-999,IBUFF(15),I,IBUFF(24),I,I,IER)
        CALL OUTCM
        END
