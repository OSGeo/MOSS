C *****************************************************************
C
C            P R O G R A M    SELECT
C FUNCTION:
C             SELECT A SET OF ITEMS FROM A MAP
C PARAMETERS:
C             LINES     IN  - THE LINE POSITION OF THE CRT CONSOLE CURSER.
C ****************************************************************
$Include:'DBCHAN.inc'
$Include:'DNAMES.inc'
$Include:'IO.inc'
$Include:'MFAST.inc'
$Include:'STRNG.inc'
$Include:'TYPE.inc'
$Include:'WHMAP.inc'
$Include:'WORK.inc'
        COMMON /SESTF/ LOCATE
        COMMON /SERIT/ JSERCH,IATCNT,KALL,NATS,INDEX,KCHAN
        COMMON /ATTFET/ KAT(86),LBAT
        COMMON /PNTFET/ NPOINT
        COMMON /UNIT/ CHANEL,ATRBTE,POLYGON,ADRESS
        INTEGER CHANEL, ATRBTE, POLYGON, ADRESS,LOCATE
        DIMENSION MAPNAME(25),ITEMS(30),infil1(12)
        LOGICAL SCOMP,NOTI,FOUND,MCOMP
C
        ITEMAX=30
        call clear(items,30)
C
c  initialize stuff
c
      call initl(ier)
      if( ier .ne. 0 ) GO TO 9999
C
C
C  SET LINES
C
        LINES=0
C
C  SET CHANNEL FOR MAP
C
        KCHAN=10
C
C  SET ATTRIBUTE SEARCH FLAG TO 0
C
        JSERCH=0
C
C  SET # OF HITS,SEARCH TYPE, AND SEARCH CRITERIA
C
        NHITS=0
        ITYPE=0
        call clear(map,25)
C
C  SAVE CURRENT STATUS OF NPOINT FOR USE TO CALL TO ADSTAD
C
        NPSAVE=NPOINT+1
C
C  SET PROMPT SWITCH
C
        IPRMT=0
C
C  open DESCRIBE.FA file
       CALL OPENF(ICDCFA,IDESFA,1,512,2,IER)
C
C  CHECK FOR MAP NAME
C
        CALL DECIP(ICARD,infil1,ICARDP)
C
C  MAP NAME SPECIFIED ?
C
        IF( infil1(1) .GT. 0 ) go to 55
C
C  NO.  PROMPT USER FOR MAP NAME
C
           IPRMT=0
    5      WRITE(NPRNT,1000)
 1000      FORMAT(' ENTER NAME OF MAP TO SELECT FROM (CR=QUIT) '\)
           CALL READIN(ICARD,LENICD,ICARDP,istop)
           IF (ICARD(1) .EQ. 0) GOTO 9999
           CALL DECIP(ICARD,Infil1,ICARDP)
C
C  CHECK FOR SPECIAL KEYWORD 'FROM'
C
55      IF(MCOMP(infil1,'FROM',4,IER)) GO TO 160
C
C  Check for valid map name
        IF (MCOMP(INFIL1,' ',4,IER)) GOTO 9999
        if(icom(9).gt.32)go to 931
        CALL MCHEK(infil1,IFIL,NENTS,KTYPE,IER)
        IF (IFIL .NE. 0) GO TO 20
C
C  MAP DOES NOT EXIST.  TELL USER
C
        WRITE(NPRNT,1001)(infil1(l),l=1,12)
 1001   FORMAT(1x,10a1,': THIS MAP DOES NOT EXIST!')
        LINES=LINES+2
        GO TO 5
C
C  THE MAP EXISTS.  PACK THE NAME, OPEN THE FILE, AND READ THE
C  HEADER.  FIRST CHECK TO MAKE SURE THAT THIS FILE HAS
C  NOT BEEN ARCHIVED.
C
  20   CONTINUE
       IF(NENTS.LT.0) GO TO 910
       IF (KTYPE .GE. 6  .AND.  KTYPE .LE. 9) IFIL = 3
C
       CALL CLEAR(MAPNAME,25)
C
       IF ( IFIL .EQ. 1 ) THEN
C flag master dir map names
          infil1(1)=(-infil1(1))
          CALL ADDDIR( Infil1, MAPNAME)
       ELSE
          CALL PACKC(Infil1,MAPNAME,10)
       ENDIF
C
C  Save map name for updating ISTRCT
       CALL PACKC(Infil1,JCOM,10)
         if(ifil.eq.3)then
           call clear(mapname,25)
           call gtnam(-1,5,infil1,mapname,ier)
         endif
       CALL OPENF(KCHAN,MAPNAME,1,256,2,IER)
       IF(IER.NE.0) GO TO 900
       CALL RDBLK(KCHAN,1,IBUFF,1,IER)
C
C CHECK PROMPT SWITCH
C
        IF(IPRMT.EQ.1) GO TO 25
C
C  NOW GET ACTION.
C
        CALL DECIP(ICARD,ICOM,ICARDP)
C
C  ANY ACTION SPECIFIED ?
C
        IF(ICOM(1).NE.0) GO TO 30
C
C  NO. PROMPT USER FOR ACTION
C
  25    CONTINUE
        WRITE(NPRNT,1002)
 1002   FORMAT(/,' ENTER ACTION: All,Subjects,Items,Features,',
     + 'Attribute,From'\)
        CALL READIN(ICARD,LENICD,ICARDP,istop)
        CALL DECIP(ICARD,ICOM,ICARDP)
        LINES=LINES+2
C
C  INTERPRET TYPE...SELECT ALL IS ONLY TYPE FOR CELL
C
  30    CONTINUE
        IF(MCOMP(ICOM,'AL',2,IER)) ITYPE=1
        IF(MCOMP(ICOM,'SU',2,IER)) ITYPE=2
        IF(MCOMP(ICOM,'IT',2,IER)) ITYPE=3
        IF(MCOMP(ICOM,'AT',2,IER)) ITYPE=4
        If(mcomp(icom,'FE',2,ier)) itype=5
        if(mcomp(icom,'FR',2,ier))itype=6
        IF (IFIL .EQ. 3) ITYPE = 1
        if(itype.lt.1.or.itype.gt.6)go to 25
C
C  CHECK FOR VALID TYPE
C
        GO TO (40,50,70,100,170,160), ITYPE
C
C  ILLEGAL ACTION SPECIFIED
C
        WRITE(NPRNT,1004)
 1004   FORMAT(/,' ILLEGAL ACTION SPECIFIED ON SELECT')
        LINES=LINES+2
        GO TO 25
C
C  SELECT ALL
C
  40    CONTINUE
        IF(IFIL.EQ.3) NHITS=1
        IF(IFIL.EQ.3) GO TO 800
        INDEX=IBUFF(64)+IBUFF(65)+2
  42    CONTINUE
        CALL LINKUP(KCHAN,INDEX,NENTS,IBUFF,1)
        NHITS = NENTS
        GO TO 800
C
C  SUBJECT SEARCH
C
  50   if(icard(icardp).gt.32)go to 51
       WRITE(NPRNT,1009)
 1009  FORMAT(' ENTER SUBJECT SEARCH STRING'\)
       CALL READIN(ICARD,LENICD,ICARDP,istop)
       if(icard(1).lt.32)go to 9999
       if(icard(1).eq.0)go to 50
  51   ISTYP=1
       IF(MCOMP(ICARD(ICARDP),'\',1,IER)) ISTYP=2
       if(istyp.eq.2)icard(icardp)=32
C
       CALL PACKC(ICARD(ICARDP),items,30)
C

C
       CALL MATCH(IBUFF,ICARD,ISTYP,NHITS,KCHAN,IER)
       ICARD(71)=(2h  )
       ICARDP=70
       GO TO 800
C
C ITEM SEARCH...DID USER SPECIFY ITEM NUMBERS ALREADY ?
C
  70    CONTINUE
        NOTI=.FALSE.
        IF(MCOMP(ICARD(ICARDP),'\',1,IER)) NOTI=.TRUE.
        IF(NOTI)then
        ipx=icardp
        do 71 ll=icardp+1,istop
        icard(ipx)=icard(ll)
        ipx=ipx+1
        icard(ll)=0
   71   continue
        endif
        N=0
        CALL INUM(ICARD,ITEMS(1),ICARDP)
        IF(ITEMS(1).GT.0) GO TO 75
C
C  NO.  PROMPT FOR ITEM NUMBER
C
        WRITE(NPRNT,1011)
 1011   FORMAT(' ENTER THE ITEM NUMBERS TO SELECT '\)
        CALL READIN(ICARD,LENICD,ICARDP,istop)
        LINES=LINES+2
        GO TO 70
C
  75    CONTINUE
        N=N+1
        IF (N.GT.ITEMAX-1) GO TO 76
        CALL INUM(ICARD,ITEMS(N+1),ICARDP)
        IF(ITEMS(N+1).GT.0) GO TO 75
C
  76    CALL ORDERI(ITEMS,N)
C
C  NOW SEARCH FOR ITEM
C
        INDEX=IBUFF(64)+IBUFF(65)+2
        NN=1
        DO 80 I=1,NENTS
          CALL RDBLK(KCHAN,INDEX,IBUFF,1,IER)
          IF(.NOT.NOTI.AND.ITEMS(NN).NE.IBUFF(5)) GO TO 79
          IF(NOTI.AND.ITEMS(NN).EQ.IBUFF(5)) NN=NN+1
          IF ( NOTI  .AND.  ITEMS(NN-1) .EQ. IBUFF(5) ) GO TO 79
            CALL LINKUP(KCHAN,INDEX,1,IBUFF,1)
            IF(.NOT.NOTI) NN=NN+1
            IF(.NOT.NOTI.AND.NN.GT.N) GO TO 82
  79      INDEX=IBUFF(1)
  80    CONTINUE
C
  82    CONTINUE
        IF(.NOT.NOTI) NHITS = NN-1
        IF(NOTI) NHITS=NENTS-NN+1
        NN=NHITS
        IF(NOTI) NN=N
        IF (NN .GT. 6) NN=6
c
c  load item numbers into array for <describe.dt> update
C        do 4 l=1,nn
C    4   map(l)=items(l)
        GO TO 800
C
C  ATTRIBUTE SELECTION
C
 100    CONTINUE
        JSERCH=1
        LOCATE=1
        CALL RDBLK(KCHAN,LOCATE,IBUFF,1,IER)
        IF(IBUFF(76).NE.1) GO TO 920
        INDEX=IBUFF(64)+IBUFF(65)+2
        CHANEL=31
        CALL GTNAM(0,4,MAPNAME,MAP,IER)
        CALL OPENF(chanel,MAP,1,256,2,IER)
        IF(IER.NE.0) GO TO 930
        CALL INITIAL
        CALL RETRVL(FOUND,IBUFF)
        IF(.NOT.FOUND) GO TO 98
C
C  SAVE ATTRIBUTE KEY FOR ACTIVE TABLE ENTRY
        CALL CLEAR(items,30)
        CALL PACKC(IBUFF(2),items,30)
C
  130   LOCATE=IBUFF(86)
        KEEPLOC=LOCATE
        IATYP=IBUFF(84)
        NATS=0
        IF(IATYP.EQ.3) IATYP=IBUFF(85)
        IF(IBUFF(84).EQ.3) IATYP=-IATYP
        IF(IBUFF(84).EQ.1) CALL DIGOUT(LOCATE)
        IF(IBUFF(84).EQ.2) CALL RLSOUT(LOCATE)
        IF(IBUFF(84).EQ.3) CALL CHROUT(IBUFF(86),IBUFF(85))
        CALL RDBLK(KCHAN,1,IBUFF,1,IER)
        INDEX=IBUFF(64)+IBUFF(65)+2
        LBAT=(NSTRCT/7)*2+1
        KAT(LBAT)=IATYP
        LBAT=LBAT+1
        KAT(LBAT)=KEEPLOC
        IF(KALL.EQ.1)  CLOSE(chanel,iostat=IER)
        NHITS=NATS
        IF(KALL.EQ.1) GO TO 42
        IF(NATS.EQ.0) GO TO 98
c
  98    CLOSE(chanel,iostat=IER)
        GO TO 800
C
C  ELEVATION SEARCH
C
c 150    CONTINUE
c        IF(KTYPE.LT.11  .OR.  KTYPE.GT.13) GO TO 940
c        CALL SELEV(IBUFF,NHITS,KCHAN,ICARD,ICARDP,LINES,
c     +             NENTS,LOW,IUP,IER)
c
c  load upper & lower elev. bounds into <describe.dt> update array
c
c        items(1)=low
c        items(2)=iup
c
c       GO TO 800
C
C  EXTERNAL FILE SEARCH
C
 160    CONTINUE
        CALL SELEXT(IBUFF,NHITS,ICARD,ICARDP,LINES,IER)
        ICARD(1)=0
        ICARDP=0
        GO TO 880
C
C  SEARCH FROM LIST OF FEATURES IN A FILE
C
 170   CONTINUE
       ITYPE = 6
       CALL SELFEA (KCHAN,IBUFF,NHITS,items,ICARD,ICARDP,LINES,IER)
       GO TO 800
C
C  FINISH UP
C
 800    CONTINUE
        IF (NHITS .LT. 1) GO TO 880
        NACTS=NACTS+1
        IDATYP(NACTS)=KTYPE
        CALL ADSTAD(IER,JCOM,NHITS,NPSAVE,items,ITYPE)
        WRITE(NPRNT,1024) nHITS,NACTS
 1024   FORMAT(1X,I5,' ITEMS FOR ACTIVE ID',I3)
        LINES=LINES+1
C
 880    CALL PUTFAST(-999,IBUFF(15),INDEX,IBUFF(24),INDEX,INDEX,IER)
        CLOSE(ICDCFA)
        IF(IFIL.NE.3) CLOSE(KCHAN,iostat=IER)
C
C
        call outcm
        GO TO 9999
C
C  ERROR RETURNS
C
 900    write(nprnt,90001)(MAPNAME(l),l=1,25)
90001   format(/,1x,'Error accessing input map -:',25a2)
        GO TO 5
 910    write(nprnt,91001)(jcom(l),l=1,10)
91001   format(/,1x,'This map has been archived to mag. tape',10a2)
        GO TO 5
 920    write(nprnt,92001)(jcom(l),l=1,10)
92001   format(/,1x,'This map has no multiple attr. file: ',10a2)
        GO TO 5
 930    write(nprnt,93001)(map(l),l=1,10)
93001   format(/,1x,'Could not open (.at) file :',10a2)
        GO TO 25
 940    WRITE(NPRNT,3040)
 3040   FORMAT(' CANNOT DO ELEVATION SEARCH ON TWO-D VECTOR FILES')
        LINES=LINES+2
        GO TO 25
C
 950    write(nprnt,3050)
 3050   format(' Could not open <Describe.FA> feature file')
        go to 25
  931   write(*,*)'File Name must be 8 characters or less'
        go to 5
C       
9999    CONTINUE
        END
