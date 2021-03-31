      Subroutine SAVWRK(ictsub,npolys,ichan8,ichan5)
C
C ***************************************************************
C
C           S U B R O U T I N E    S A V W R K
C FUNCTION:
C             THIS ROUTINE ADDS A DATA SET TO A MOSS WORKFILE DATA BASE.
C             THE INPUT DATA IS ON A TEMPORSRY SAVE DATA FILE.
C ROUTINES
C CALLED:     INCM,PUTSB,PPERIM,OUTCM,RDBLK,POLCEN,BACK,CFILW,,
C             PUTRX,RLNLEN,COWRT,WRBLK,PUTRY,RESET,CHASH,CENTAR,CLEAR.
C
C RECORD LAYOUT FOR A MOSS RECORD:
C
C       WORD 1   = LINK TO NEXT RECORD
C       WORD 2   = LENGTH OF THIS RECORD IN RECORDS
C       WORD 3   = SUBJECT LINK
C       WORD 4   = CATEGORY LINK
C       WORD 5   = ITEM NUMBER
C       WORD 6   = TYPE =
C                           1=POLYGON
C                           2=LINE
C                           3=POLYGON
C                           5=ELEVATION
C       WORD 7/8 = AREA IN ACRES
C       WORD 9/10= LENGTH OR PERIMETER IN MILES
C       WORD11/12= X CENTROID FOR LINE OR POLYGON
C       WORD13/14= Y CENTROID FOR LINE OR POLYGON
C       WORD15/16= XMIN
C       WORD17/18= XMAX
C       WORD19/20= YMIN
C       WORD 23  = POINTER TO CATEGORY RECORD
C       WORD 24  = POINTER TO SUBJECT RECORD
C       WORD21/22= YMAX
C ******************************************************************
C
$Include:'io.inc'
$Include:'xypnts.inc'
$Include:'pqpnts.inc'
         common/tempsort/tempbuff(128),iptr,iwrite,lastrec
        DIMENSION KBUFF(128),ICOORD(4),
     +           MARRY(44),IPITEM(2),NAME1(25),NAME2(25),NAME(10)
        DIMENSION IARR(128),IXT(2),IYT(2)
        DIMENSION IXMIN(2),IXMAX(2),IYMIN(2),IYMAX(2)
        DIMENSION IARR6(5)
        DIMENSION ISLAND(353),ISUB(15),map(25)
        REAL PITEM
        character mar1*3, mar2*3
        LOGICAL MCOMP
        EQUIVALENCE (ICOORD(1),XT),(ICOORD(3),YT)
        EQUIVALENCE (IXT,XT),(IYT,YT)
        EQUIVALENCE (IXMIN,XMIN),(IXMAX,XMAX),(IYMIN,YMIN),(IYMAX,YMAX)
        EQUIVALENCE (IPITEM(1),PITEM)
C
C  SET ITEM AND ISLAND COUNTERS
C
        LKUT=0
        NSLAND=0
        ITEMS=0
        iptr=0
        iwrite=0
        lastrec=0
C
C  READ IN NEW MAP NAME
C
        rewind(ichan8)
        read(ICHAN8,iostat=ier,err=8)name
    8   If(ier.ne.0)write(nprnt,*)' error reading new map data ',ier
C
C  MAKE SURE 11TH CHARACTER IS NULL
C
C  READ THE NAMES OF THE MAPS THIS ONE DERIVED FROM
C
        read(ICHAN8)NAME1
C
C  MAKE SURE 11TH CHARACTER IS NULL
C
        read(ICHAN8)NAME2
C
C  MAKE SURE 11TH CHARACTER IS NULL
C
C  READ THE MAP DESCRIPTION FIELDS FOR THE MAP HEADER
C
        read(ICHAN8)(IARR(ll),ll=1,65)
C
C  COORDINATE CONVERSION FACTOR
C
        ICONV=0
        ICVAL=IARR(65)
        IF(MCOMP(IARR(65),'F',1,IER)) ICONV=1
C
C  SEE IF MUTIPLE ATTS FLAG SET...IF SO READ COUNTS AND ID'S, AND OPEN ARRAY
C
        map(1)=2hB
        IMA = 0
        IF (IARR(1) .NE. -32767) GO TO 10
        read(ICHAN8)(MARRY(ll),ll=1,44)
        IF ( (MARRY(1) .GT. 0  .OR.  MARRY(1) .EQ. -1)  .OR.
     +       (MARRY(2) .GT. 0  .OR.  MARRY(2) .EQ. -1) ) IMA = 1
C        IF (IMA .EQ. 1) CALL OPENF(ICHP,map,3,256,2,IER)
        IF (IER .NE. 0) GO TO 909
        IRBGP = 1
C
C  SET ACTUAL SUBJECT AND CATEGORY COUNTTS TO ZERO
C
  10    NNCAT=0
        NNSUB=0
C
C.......SORT THE TEMPSUB FILE ON THE FIRST PASS .....
C
C        IF (SORTED) GO TO 100
C.........OPEN TEMP FILE THAT SUBJECTS WILL BE SORTED INTO
          ichsrt=100
          map(1)=2ha
          CALL OPENF(ichsrt,map,3,256,2,ier)
          nsub=npolys
          CALL SRTTMP (ICtsub,ICHsrt,NSUB,NNSUB,IER)
            IF (IER .NE. 0) GO TO 80030
C          SORTED = .TRUE.
C
C  CREATE AND OPEN FILE THEN WRITE HEADER  < opened in calling>
C
C        CALL OPENF(ICHAN5,NAME,0,256,2,IER)
C        IF(IER.NE.0) GO TO 901
        CALL CLEAR(IARR(68),60)
        IARR(64)=1
        NCATS=IARR(64)
        IARR(77) = IARR(65)
        IARR(65) = NNSUB
        NSUBS=IARR(65)
        IARR(66) = 0
        CALL WRBLK(ICHAN5,1,IARR,1,IER)
C
C  GET DATA TYPE
C
        IDTYPE=0
        read(ICHAN8)IDTYPE
        WRITE(NPRNT,1089) IDTYPE
 1089   FORMAT(' BUILD PC-MOSS MAP DATA TYPE ',I4)
        IF (IMA .EQ. 1) then
         write(mar1,'(i3)')marry(1)
         write(mar2,'(i3)')marry(2)
         if(marry(1).eq.-1)mar1='ALL'
         if(marry(2).eq.-1)mar2='ALL'
       WRITE(NPRNT,1090) MAR1,(NAME1(I),I=1,25),MAR2,
     +   (NAME2(I),I=1,25)
 1090   FORMAT(' SAVE ',a3,' ATTRIBUTES FROM ',25A2,/,
     +         '  AND ',a3,' ATTRIBUTES FROM ',25A2)
        endif
C
C  CALCULATE ADDRESS OF FIRST POLYGON RECORD
C
        NREC=NCATS+NSUBS+2
C
C  WRITE A DUMMY RECORD TO PREVENT EOF
C  ON SUBJECT AND CATTEGORY TABLE BUILDS
C
        CALL WRBLK(ICHAN5,NREC,IARR,1,IER)
C
C  INITIALIZE MAP MBR
C
        XXMIN=9999999.
        YYMIN=XXMIN
        XXMAX=-XXMIN
        YYMAX=XXMAX
C
C  ENTER MAIN DATA TRANSFER LOOP. READ CATEGORY, SUBJECT,
C  AND NUMBER OF COORDINATE PAIRS
C
 100    CONTINUE
c        READ(ichan8,END=200,ERR=200)
         read(ichan8,end=200,iostat=ier)
     1  (ISUB(I),I=1,15),(IARR6(I),I=1,5),NVERT,NSLAND
        IF(NVERT.EQ.0) GO TO 200
        IF(NSLAND.GT.353) GO TO 911
        IF(NSLAND.EQ.0) GO TO 25
        read(ICHAN8)(ISLAND(ll),ll=1,NSLAND)
  25    CONTINUE
        IF(IDTYPE.EQ.5) IARR(5)=IARR(3)
  20    CONTINUE
        IF(NSLAND.EQ.0) ISLAND(1)=0
C
C  INCREMENT ITEM COUNT
C
        ITEMS=ITEMS+1
C
C
C  SAVE FEATURE NUMBERS FOR MULTIPLE ATTRIBUTES IF REQUIRED
C
        IF (IMA .EQ. 1) IPITEM(1) = IARR6(1)
        IF (IMA .EQ. 1) IPITEM(2) = IARR6(2)
        if(ima.eq.1) then
               call spoints('P ',p,pitem,items,2)
c              p(items)=pitem
        endif
C
        XMIN=9999999.0
        YMIN=XMIN
        XMAX=-XMIN
        YMAX=XMAX
        IPOINT=0
        DO 21 I=1,NVERT
        read(ICHAN8)xt,yt
C
C grab first point
C
          if(i.eq.1)then
            xttx=xt
            ytty=yt
          endif
C
C  FEATURE MBR
C
        XMIN=AMIN1(XMIN,XT)
        YMIN=AMIN1(YMIN,YT)
        XMAX=AMAX1(XMAX,XT)
        YMAX=AMAX1(YMAX,YT)
C
C  MAP MBR
C
        XXMIN=AMIN1(XXMIN,XT)
        YYMIN=AMIN1(YYMIN,YT)
        XXMAX=AMAX1(XXMAX,XT)
        YYMAX=AMAX1(YYMAX,YT)
C        CALL PUTRX(IPOINT,XT)
C        CALL PUTRY(IPOINT,YT)
        ipoint=ipoint+1
        call spoints('X ',x,xt,ipoint,2)
        call spoints('Y ',y,yt,ipoint,2)
c        x(ipoint)=xt
c        y(ipoint)=yt
C
C make last spot
C
         xlast=xt
         ylast=yt
         lastpt=ipoint
C
  21    CONTINUE
C
  26    CONTINUE
C
C if polygon make sure closes
C
        if(idtype.eq.3)then
         if(xttx.ne.xlast.or.ytty.ne.ylast)then
           call spoints('X ',x,xttx,lastpt,2)
           call spoints('Y ',y,ytty,lastpt,2)
         endif
        endif
C
        IPOINT=NSLAND
C
  30    CONTINUE
C
C  ZERO OUT FIRST 4 ELEMENTS OF IARR
C
        IARR(1)=0
        IARR(2)=0
        IARR(3)=0
        IARR(4)=0
C
C  PUT SUBJECT IN TABLE
C
      CALL PUTSUB(IARR,NREC,ISUB,NNSUB,ICHAN5,ichsrt,IER)
C
C  STORE ITEM NUMBER
C
        IARR(5)=ITEMS
C
C  STORE TYPE
C
        IARR(6)=IDTYPE
C
C  CALCULATE THE AREA IF IT IS A POLYGON
C
C
        IF(IDTYPE.EQ.3)
     +   CALL CENTAR(NVERT,XCEN,YCEN,XT,ISLAND,NSLAND,ICONV)
        IF(IDTYPE.EQ.3.AND.XT.LE.0.0001) LKUT=LKUT+1
C
C  IF LINE, CALCULATTE THE LENGTH
C
        IF(IDTYPE.EQ.2) XT=RLNLEN(NVERT)
        IF (IDTYPE.EQ.2 .AND. ICONV .EQ. 1) XT = XT * 3.280827663
C
C  STORE AREA OR LENGTH
C
        IARR(7)=IXT(1)
        IARR(8)=IXT(2)
C
C  IF POLYGON, CALCULATE THE PERIMETER
C
        IF(IDTYPE.NE.3) GO TO 41
        CALL PPERIM(NVERT,ISLAND,IPOINT,XT)
        IF (ICONV .EQ. 1) XT = XT * 3.280827663
        IARR(9)=IXT(1)
        IARR(10)=IXT(2)
  41    CONTINUE
C
C  IF POLYGON LOAD THE CENTROID
C
        IF(IDTYPE.NE.3) GO TO 42
        XT=XCEN
        YT=YCEN
        IARR(11)=IXT(1)
        IARR(12)=IXT(2)
        IARR(13)=IYT(1)
        IARR(14)=IYT(2)
  42    CONTINUE
C
C  STORE MINS AND MAXES
C
        IARR(15)=IXMIN(1)
        IARR(16)=IXMIN(2)
        IARR(17)=IXMAX(1)
        IARR(18)=IXMAX(2)
        IARR(19)=IYMIN(1)
        IARR(20)=IYMIN(2)
        IARR(21)=IYMAX(1)
        IARR(22)=IYMAX(2)
C
C  STORE NUMBER OF COORD PAIRS AND NUMBER OF ISLANDS
C
        IARR(25)=NVERT
        IARR(26)=NSLAND
C
C  STORE ISLAND POINTERS
C
        INC=1
        KPOINT=27
        DO 45 II=1,NSLAND
        IARR(KPOINT)=ISLAND(II)
        KPOINT=KPOINT+1
        ISLAND(II) = 0
  45    CONTINUE
C
C  WRITE OUT FIXED FIXED LENGTH PORTION OF RECORD
C
       CALL WRBLK(ICHAN5,NREC,IARR,1,IER)
       IF(NSLAND.LE.99) GO TO 48
       ISL = (IARR(26)-100)/128 + 1
       ICOUNT = 0
       DO 47 JJJ=1,ISL
       DO 46 II=1,128
       KBUFF(II)=ISLAND(II+99+ICOUNT)
46     CONTINUE
       ICOUNT = ICOUNT + 128
       CALL WRBLK(ICHAN5,NREC+JJJ,KBUFF,1,IER)
  47   CONTINUE
       INC = INC + ISL
C
C  STORE CURRENT RECORD POINTTER FOR RECORD LINK CONSTRUCTION
C
48      NNREC=NREC
C
C  INCREMENT RECORD COUNTER
C
        NREC=NREC+INC
C
C  STORE COORDINATE DATA
C
        IZZ=0
        CALL COWRT(IARR,NVERT,NREC,XMIN,YMIN,ICHAN5,IZZ)
C
C NOW READ IN FIRST RECORD FOR THIS ITEM AND UPDATE IT WITH
C THE ITEM LINK AND THE ITEM LENGTH (IN RECORDS)
C
        CALL RDBLK(ICHAN5,NNREC,IARR,1,IER)
        NR=NREC-NNREC
        IARR(2)=NR
        IARR(1)=NREC
        CALL WRBLK(ICHAN5,NNREC,IARR,1,IER)
        GO TO 100
C
C  ALL DONE
C  ZERO LAST LINK POINTER TO PREVENT POSSIBLE EOF ERRORS IN COS
C
 200    CONTINUE
        CALL RDBLK(ICHAN5,NNREC,IARR,1,IER)
        IARR(1)=0
        CALL WRBLK(ICHAN5,NNREC,IARR,1,IER)
C
C  UPDATE ACTUAL SUBJECT AND CATEGORY COUNTERS
C
        CALL RDBLK(ICHAN5,1,IARR,1,IER)
        IARR(1) = NREC - 1
        IARR(62) = IDTYPE
        IARR(63) = ITEMS
        IARR(66)=NNCAT
        IARR(67)=NNSUB
        IARR(77)=ICVAL
C
C  STORE MAP MBR IN HEADER
C
      XMIN=XXMIN
      YMIN=YYMIN
      XMAX=XXMAX
      YMAX=YYMAX
      IARR(68)=IXMIN(1)
      IARR(69)=IXMIN(2)
      IARR(70)=IXMAX(1)
      IARR(71)=IXMAX(2)
      IARR(72)=IYMIN(1)
      IARR(73)=IYMIN(2)
      IARR(74)=IYMAX(1)
      IARR(75)=IYMAX(2)
      CALL WRBLK(ICHAN5,1,IARR,1,IER)
C
C  close in calling
C      CLOSE(ICHAN5,iostat=IER)
C
C  DO MULTIPLE ATTS IF REQUIRED
C
       IF (IMA .EQ. 1)then
       ichmap=41
       CALL SAVMAT(ITEMS,MARRY,NAME,NAME1,NAME2,ICHan5,ICHmap,IER)
       endif
C
C  ALL DONE
C
 999  CONTINUE
        IF(LKUT.GT.0)
     +write(nprnt,*) ' WARNING ',LKUT,
     + ' POLYGONS ARE LESS THAN .0001 ACRES'
      return
C
C  ERROR RETURNS
C
 898   WRITE(NPRNT,90898) IER
90898  FORMAT('*SAVWRK*  ERROR FROM OPEN OF THE TEMP FILE  ERROR',I5)
       GO TO 999
899    WRITE(NPRNT,90899) IER
90899  FORMAT('*SAVWRK*  ERROR FROM RDSEQ OF TEMP FILE  ERROR',I5)
       GO TO 999
 900   WRITE(NPRNT,3000) (NAME(I),I=1,5),IER
 3000  FORMAT(' *SAVWRK*  ERROR FROM CREATE OF FILE ',5A2,'  ERROR',I5)
       GO TO 999
 901   WRITE(NPRNT,3001) (NAME(I),I=1,5),IER
 3001  FORMAT(' *SAVWRK*  ERROR FROM OPEN OF FILE ',5A2,'  ERROR',I5)
       GO TO 999
 905   WRITE(NPRNT,3005) IER
 3005  FORMAT('  *SAVWRK*  ERROR FROM PUTSUB ',I5)
       GO TO 999
 909   WRITE(NPRNT,3010) IER
 3010  FORMAT(' *SAVWRK*  ERROR FROM OPEN OF DISK ARRAY FILE  ERROR',I5)
       GO TO 999
 911   WRITE(NPRNT,3011) NSLAND
 3011  FORMAT(' *SAVWRK*  MORE THAN 353 ISLANDS THERE ARE',I6)
       GO TO 999
80030 write(nprnt,*)' ** error in sorting temp subject file'
      go to 999
C
       END
