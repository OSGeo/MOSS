$Include:'mfast.inc'
$Include:'dbchan.inc'
$Include:'io.inc'
$Include:'work.inc'
$Include:'strng.inc'
$Include:'vwnd.inc'
$Include:'dnames.inc'
$Include:'xypnts.inc'
$Include:'whmap.inc'
        common /output/ buffer(132),ipt,iwrd,string(2)
        COMMON /FLS/    IUNIT,IFILE(5)
C
        DIMENSION WNDW(4)
        DIMENSION IHEAD(256)
        DIMENSION IARR(128),IXT(2),IYT(2)
        DIMENSION JXMIN(2),JYMIN(2),JXMAX(2),JYMAX(2)
        DIMENSION IXMIN(2),IXMAX(2),IYMIN(2),IYMAX(2)
        DIMENSION ITYPE(10),TX(15),TY(15),SX(15),SY(15)
        DIMENSION IARR6(5),KHASH(100),ITSUB(100)
        DIMENSION ISLAND(100),ISUB(15), LABEL(15)
        integer fchan,wrk(10),buffer,string,mapname(25)
        Integer meters,feet
        integer*2 iyear, imonth, iday
C
        EQUIVALENCE (IXT,XT),(IYT,YT)
        EQUIVALENCE (JXMIN,XXMN),(JXMAX,XXMX),(JYMIN,YYMN),(JYMAX,YYMX)
        EQUIVALENCE (IXMIN,XMIN),(IXMAX,XMAX),(IYMIN,YMIN),(IYMAX,YMAX)
C
        LOGICAL MCOMP, MCOMP1, KEY, DIGITZ, NOPROJ, ido
        DATA IBLNK/10H          /
        data meters/2hM /
        data feet/2hF /
C
C ************************************************************************
C
C           S U B R O U T I N E    G E N E R A T E
C
C  FUNCTION - ADD A MAP TO A MOSS POLYGON WORKFILE USING EITHER
C             DIGITIZER INPUT, CURSOR INPUT OR KEYBOARD ENTRY.
C             (Digitizer input currently disabled).
C
C *************************************************************************
C
C
C  READ IN COMMON BLOCK INFO
C
      CALL initl(ier)
      if(ier.ne.0)stop
c
C
C GET INPUT/OUTPUT UNITS
C
c
      map(1)=2HLM
      map(2)=2h
      jchan=81
      call openf(jchan,map,1,0,1,ier)
      if(ier.ne.0)then
         write(nprnt,2)
    2    format(/,1x,'You must do the <PLOT> command first')
         return
      endif
c
      map(1)=2hZ
      call openf(70,map,3,0,1,ier)
c
      write(*,*)char(27),char(12)
      call initt(1,iunit,ifile)
C
C  CHECK FOR INPUT FROM GRAPHICS CURSOR OR FROM DIGITIZING TABLE
C**
C    (digitizer input disabled in this version)
C
      NOPROJ = .TRUE.
      DIGITZ=.FALSE.
c**
C      IF(ICARD(80).EQ.1) DIGITZ=.TRUE.
C      IF(.NOT.DIGITZ) GO TO 1
C
C      ICHAND=18
c      CALL DIGOPN ( ICHAND, DIGITZ, IER )
C      IF(IER.NE.0) GO TO 999
c      CALL DIGSET ( SX, SY, TX, TY, NCONT, ICHAND, IER )
C      IF (IER .NE. 0) GO TO 999
    1 CONTINUE
C
C  SET SCREEN WINDOW AND VERIFY THAT IT IS  VALID
C
      CALL SETWND
C
C  check for zero window
C
      CALL WHWND ( WNDW(1), WNDW(2), WNDW(3), WNDW(4) )
      IF (WNDW(3)-WNDW(1).EQ.0. .OR. WNDW(4)-WNDW(2).EQ.0.) GO TO 909
C
      MAXSUB=100
C
C  SET ITEM COUNTER AND CURSOR COUNTER
C
      LINES=1
      NSLAND=0
      ITEMS=0
C
C  BOOKKEEPING
C
      CALL CLEAR ( IHEAD(1), 256 )
      CALL FILLC ( IHEAD, 64, (2H  ) )
      CALL CLEAR ( KHASH, 100 )
      CALL CLEAR ( ITSUB, 100 )
      NNCAT=0
      NNSUB=0
C
      XXMN=9999999.
      YYMN=XXMN
      XXMX=-XXMN
      YYMX=XXMX
C
    4 CONTINUE
C
      write(*,9897)
9897  format(' OUTPUT MAP NAME ',\)      
      call readin(label,15,ic,iend)
      
c
      CALL MCHEK ( LABEL, IFIL, NENTS, ITYPE, IER )
      IF(IFIL.gt.0) then
C
       write(*,*)'This map already exists'
C       call readin(iget,1,ic,iend)
C       if(iget.eq.ichar('Y').or.iget.eq.ichar('y'))then
C        go to 5
C       else
        go to 4
       endif
C
C      else
C
C       if(ifil.eq.1)then
C        write(*,*)'This map exists in MASTER dir. - can not add to it'
C        go to 4
C       endif
C      endif
    5 CONTINUE
C
       CALL PACKC ( LABEL, ISUB, 15 )
       IZ=0
      ichan5=15
C
C  WHAT ABOUT ANOTHER MAPS HEADER AS A TEMPLATE???
C
  55  continue
C
       write(*,*)
       write(*,9896)
9896   FORMAT(' USE LAST MAP PLOTTED AS STARTING POINT (Y/N) ?'\)
       call readin(iget,1,ic,iend)
       ido=.false.
       if(iget.eq.ichar('Y').or.iget.eq.ichar('y'))ido=.true. 
C
C open template only if generating in a NEW file
C
       if(ifil.eq.0) then
C
       WRITE(NPRNT,1053)
 1053  FORMAT(' ENTER MAPNAME FOR HEADER TEMPLATE: (cr=none) '\)
c       LINES = LINES + 1
       CALL READIN ( ICARD, lenicd, ic, iend )
       if(icard(1).lt.32)then
          go to 8
       else 
         CALL DECIP  ( ICARD, JCOM, ic )
C         
C  CHECK FOR VALID MAP NAME
C
       CALL MCHEK ( JCOM, IFIL2, NENTS, KTYPE, IER )
       IF (IFIL2 .NE. 0) GO TO 6
C
C  MAP DOES NOT EXIST.  TELL USER
C
       WRITE(NPRNT,1001) JCOM
 1001  FORMAT(' THE TEMPLATE MAP - ',10a1,' - DOES NOT EXIST!')
       GO TO 55
C
C  OPEN TEMPLATE MAP FOR HEADER
C
    6  CONTINUE
c
       CALL PACKC ( JCOM, ICOM, 10 )
C
       if(ifil2.eq.1) then
        icom(1)=(-icom(1))
        call adddir(icom,mapname)
       else
        call adddir(icom,mapname)
       endif
       CALL OPENF ( ICHAN5, mapname, 1, 256, 2, IER )
       if (ier.ne.0) go to 901
       CALL RDBLK ( ICHAN5, 1, IHEAD(1), 1, IER )
       CALL RDBLK ( ICHAN5, 2, IHEAD(129), 1, IER )
       CLOSE ( ICHAN5,iostat=IER )
       NOPROJ = .FALSE.
C
       WRITE(NPRNT,1055) (IHEAD(ll),ll=20,50)
 1055  FORMAT(1x,30a2)
c       LINES = LINES + 1
C
C  NEW DESCRIPTION OR USE OLD ONE
C
      end if
c
 8      if(noproj)WRITE(NPRNT,1077)
 1077 FORMAT(60(1H-),/,' ENTER A DESCRIPTION')
c      LINES = LINES + 3
      IF ( .NOT.NOPROJ ) WRITE(NPRNT,1078)
 1078 FORMAT(' ENTER A DESCRIPTION OR (CR) TO RETAIN TEMPLATES', 
     +' DESCRIPTION'\)
C      WRITE(NPRNT,1080)
 1080 FORMAT(' ')
      CALL FILLC ( IARR, 128, (2H  ) )
      call readin(iarr,128,ic,iend)
      if(iarr(1).lt.32)go to 9
      DO 85 I = 20, 49
   85   IHEAD(I) = IARR(I)
C
      endif
C
C  PUT HEADER INTO NEW MAP FILE
C
C  create map only if it does not already exist
C
    9 CALL OPENF ( ICHAN5, ISUB, 2, 256, 2, IER )
      IF(IER.NE.0) GO TO 901
C
C
C pull in existing header if map already exists
C
      if(ifil.eq.0)then
      IHEAD(50) = 0
      IHEAD(58) = 0
      IHEAD(59) = 0
      IHEAD(60) = 0
      IHEAD(61) = 0
      IHEAD(64)=1
      IHEAD(65) = MAXSUB
      IHEAD(66)=0
      IHEAD(67)=0
      IHEAD(76) = 0
      iunits=0
      if(ihead(77).eq.ichar('M'))iunits=1
C
        IHEAD(12) = iyear
        IHEAD(13) = iyear
        IHEAD(14) = imonth
        IHEAD(15) = iday
C
      else
      call rdblk(ichan5,1,ihead(1),1,ier)
      call rdblk(ichan5,2,ihead(129),1,ier)
      endif
C
   10 NCATS=IHEAD(64)
      NSUBS=IHEAD(65)
C
C  CALCULATE ADDRESS OF FIRST POLYGON RECORD
C
      NREC=NCATS+NSUBS+2
C
C  WRITE A DUMMY RECORD TO PREVENT EOF
C  ON SUBJECT AND CATEGORY TABLE BUILDS
C
      if(ifil.eq.0)then
      IF ( NOPROJ ) CALL CLEAR ( IHEAD(129), 128 )
      IF ( NOPROJ ) IHEAD(193) = -1
      CALL WRBLK ( ICHAN5,    1, IHEAD(  1), 1, IER )
      CALL WRBLK ( ICHAN5,    2, IHEAD(129), 1, IER )
      CALL CLEAR ( IHEAD(129), 128 )
      CALL WRBLK ( ICHAN5, NREC, IHEAD(129), 1, IER )
      endif
C
C  AVOID MIXING DATA TYPES IN SAME MAP
C
 1000 CONTINUE
C
      WRITE(NPRNT,1003)
 1003 FORMAT(' ENTER DATA TYPE THIS MAP IS TO BE:'/'      1 - POINT'/
     1  '      2 - LINE'/'      3 - POLYGON'\)
c      LINES = LINES + 5
      CALL READIN ( ICARD, lenicd, ic, iend )
      CALL INUM ( ICARD, IDTYPE, ic )
      IF( IDTYPE.GE.1 .AND. IDTYPE.LE.3 ) GO TO 1010
      WRITE(NPRNT,1005) IDTYPE
 1005 FORMAT(' DATA TYPE ',I3,' IS INVALID HERE.  PLEASE RE-ENTER<7>'/)
c      LINES = LINES + 2
      GO TO 1000
C
C  ASK FOR KEYBOARD OR CURSOR
C
 1010 KEY=.FALSE.
      IF(DIGITZ) GO TO 15
C
      WRITE(NPRNT,40004)
40004 FORMAT(' KEYBOARD OR CURSOR INPUT (K/C) '\)
c      LINES = LINES + 1
      CALL READIN ( ICARD, lenicd, ic, iend )
      IF(MCOMP(ICARD,'K',1,IER)) KEY=.TRUE.
      IF(.NOT.(KEY.OR.MCOMP(ICARD,'C',1,IER))) GO TO 1010
  15  CONTINUE
C
C  ENTER MAIN DATA TRANSFER LOOP. READ CATEGORY, SUBJECT,
C  AND NUMBER OF COORDINATE PAIRS
C
  100 CONTINUE
c
c  screen control.  set to alpha mode
c
c       write(nprnt,*) char(27),char(12)
C
  20  CONTINUE
C
C  SET MIN AND MAXS
      XMIN=9999999.
      YMIN=XMIN
      XMAX=-XMIN
      YMAX=-YMIN
C
C  SET ISLAND COUNTER
C
      CALL CLEAR ( IARR, 128 )
      CALL CLEAR ( ISLAND, 100 )
      IPOINT=0
C
C  ASK USER WHAT TYPE OF POLYGON TO GENERATE (CIRCLE, QUIT, OR POLYGON)
C
  29  CONTINUE
c**
c
        igen=0
   30 CONTINUE
      IF (ITEMS+1 .GT. MAXSUB) WRITE (NPRNT,90030) MAXSUB
90030 FORMAT(' MAXIMUM NUMBER OF FEATURES IS',I5)
      IF (ITEMS+1 .GT. MAXSUB) GO TO 200
C
C  GET SUBJECT FROM USER
C
c* 
      call fillc (isub, 15, (2h  ))
      write(*,*)char(27),char(12)
      WRITE(NPRNT,3434)
 3434 FORMAT(' ENTER SUBJECT FOR THIS FEATURE or CR=Quit'\)
      CALL READIN ( icard, 30, ic, iend )
C
C test if quit
C
      if(icard(1).lt.32)go to 200
c*
      CALL PACKC ( ICARD, ISUB, 30 )
C
C  PUT SUBJECT IN SUBJECT TABLE
C
      items=items+1
      nsub=nsub+1
      CALL PUTSUB1 ( IARR, NREC, ISUB, KHASH, ITSUB, MAXSUB, ICHAN5,
     1               IER )
C
        IF(IDTYPE.EQ.3) then
         WRITE(NPRNT,1048)
 1048    FORMAT(' PLEASE ENTER POLYGON FEATURE TYPE ',
     1 /     '    POLYGON (P), RECTANGLE (R), CIRCLE (C)'\)
c
C         WRITE(NPRNT,1049)
C 1049    FORMAT(' QUIT (Q) TO EXIT:  '\)
c
c      LINES = LINES + 2
       CALL READIN(ICARD,lenicd,ic,iend)
c       IGEN=0
       IF(MCOMP(ICARD(1),'R',1,IER)) IGEN=1
       IF(MCOMP(ICARD(1),'C',1,IER)) IGEN=2
       IF(MCOMP(ICARD(1),'Q',1,IER)) IGEN=3
       IF(MCOMP(ICARD(1),'P',1,IER)) IGEN=4
       IF(IGEN.EQ.0 .AND. IDTYPE.EQ.3) GO TO 29
c
C  CHECK FOR END
C
       IF(IGEN.EQ.3) GO TO 200
       endif
C
C  IF POINT OR LINE DATA TYPE AND NOT QUITTING . . .
      IF ( IDTYPE.NE.3 ) IGEN=4
C
C  INCREMENT ITEM COUNT
C
C  STORE ITEM NUMBER
C
   31 CONTINUE
      IARR(5)=ITEMS
C
      GO TO  ( 10000, 20000, 200, 40000 ),IGEN
C
C  RECTANGLE
C
10000 CONTINUE
      IPOINT=0
      IF ( KEY ) GO TO 10010
c**
C       write(nprnt,*) char(27),char(12)
c
c      CALL CPLAC ( LINES )
      write(nprnt,10001)
10001 format(//,' POINT TO LL CORNER AND HIT SPACE-BAR.'
     */,' THEN POINT TO UR CORNER AND HIT SPACE-BAR.')
      write(nprnt,6012)
 6012 format(//,1x,'( CR ) TO PROCEED OR ( Q ) TO QUIT'\)
      call readin(iget,1,ic,iend)
      if(iget.eq.ichar('Q')) go to 200
c
c  screen control.  set to graphics mode.
c
       if(.not.key)then 
       rewind(jchan)
       write(nprnt,*) char(27),char(12)
       if(ido) call drfast(jchan,ier)
       rewind(70)
       call drfast(70,ier)
       backspace(70)
       endif
c
c      LINES = LINES + 1
c**      IF(DIGITZ) CALL DIGZIN ( X1, Y1, NT, ICHAND, IER )
c**      IF(DIGITZ) CALL DWRAP ( X1, Y1, NCONT )
      IF(.NOT.DIGITZ) CALL VCURSR ( NT, X1, Y1 )
c**
      call xxx(x1,y1)
c**
      GO TO 10015
C
10010 CONTINUE
c      CALL CPLAC ( LINES )
      WRITE(NPRNT,10013)
10013 FORMAT(' ENTER LL X,Y OR E,N COORDINATES '\)
c      LINES = LINES + 1
      CALL READIN ( ICARD, lenicd, ic, iend )
      CALL FNUM ( ICARD, 80, X1, ic )
      CALL FNUM ( ICARD, 80, Y1, ic)
c
      call xxx(x1,y1)
10015 CONTINUE
      IF ( KEY ) GO TO 10020
      IF(.NOT.DIGITZ) CALL VCURSR ( NT, X2, Y2 )
c**
      call xxx(x2,y2)
      GO TO 10025
C
10020 CONTINUE
C      CALL CPLAC ( LINES )
      WRITE(NPRNT,10023)
10023 FORMAT(' ENTER UR X,Y OR E,N COORDINATES '\)
c      LINES = LINES + 1
      CALL READIN ( ICARD, lenicd, ic, iend )
      CALL FNUM ( ICARD, 80, X2, ic )
      CALL FNUM ( ICARD, 80, Y2, ic )
c**
      call xxx(x2,y2)
10025 CONTINUE
      NVERT=5
      call spoints('X ',x,x1,1,2)
      call spoints('X ',x,x1,2,2)
      call spoints('X ',x,x2,3,2)
      call spoints('X ',x,x2,4,2)
      call spoints('X ',x,x1,5,2)
      call spoints('Y ',y,y1,1,2)
      call spoints('Y ',y,y2,2,2)
      call spoints('Y ',y,y2,3,2)
      call spoints('Y ',y,y1,4,2)
      call spoints('Y ',y,y1,5,2)
c      X(1)=X1
c      Y(1)=Y1
c      X(2)=X1
c      Y(2)=Y2
c      X(3)=X2
c      Y(3)=Y2
c      X(4)=X2
c      Y(4)=Y1
c      X(5)=X1
c      Y(5)=Y1
      CALL DRWSYM ( NVERT )
      CALL FLSH
      write(*,*)char(27),char(12)
      GO TO 88888
C
C  CIRCLE
C
20000 CONTINUE
c**
C      write(nprnt,*) char(27),char(12)
c**
      IPOINT=0
      IF ( KEY ) GO TO 20020
c**
      WRITE(NPRNT,20001)
20001 FORMAT(' POINT TO CENTER OF CIRCLE, HIT SPACE-BAR, THEN')
      if(iunits.eq.0) write(nprnt,20002)
20002 format(' ENTER RADIUS IN MILES. ')
      if(iunits.eq.1) write(nprnt,20022)
20022 format(' ENTER RADIUS IN KILOMETERS. ')
      write(nprnt,6012)
      call readin(iget,1,ic,iend)
      if(iget.eq.ichar('Q')) go to 200
c*
      if (.not.key) then
       rewind(jchan)
       write(nprnt,*) char(27),char(12)
       if(ido) call drfast(jchan,ier)
       rewind(70)
       call drfast(70,ier)
       backspace(70)
       endif
c*     
c**      IF(DIGITZ) CALL DIGZIN ( XCEN, YCEN, NT, ICHAND, IER )
c**      IF(DIGITZ) CALL DWRAP ( XCEN, YCEN, NCONT )
      IF(.NOT.DIGITZ) CALL VCURSR ( NT, XCEN, YCEN )
c**
      call xxx(xcen,ycen)
      GO TO 30993
C
20020 CONTINUE
c**
      call anmode
      write(*,*)char(27),char(12)
      WRITE(NPRNT,20023)
20023 FORMAT(' ENTER CIRCLE CENTER X,Y OR E,N COORDINATES '\)
      LINES = LINES + 1
      CALL READIN ( ICARD, lenicd, ic, iend )
      CALL FNUM ( ICARD, 80, XCEN, ic )
      CALL FNUM ( ICARD, 80, YCEN, ic )
C
20003 CONTINUE
c*
      IF(IUNITS.EQ.0) WRITE(NPRNT,30092)
30092 FORMAT(' ENTER RADIUS IN MILES '\)
      IF(IUNITS.EQ.1) WRITE(NPRNT,30093)
30093 FORMAT(' ENTER RADIUS IN KILOMETERS '\)
c      LINES = LINES + 1
C
30993 continue
      
      write(nprnt,*) 'RADIUS:'
      CALL READIN ( ICARD, lenicd, ic, iend) 
      CALL FNUM ( ICARD, 80, RADIUS, ic )
      IF(RADIUS.LE.0) GO TO 20003
      IF(IUNITS.EQ.0) RADIUS=RADIUS*1609.0
      IF(IUNITS.EQ.1) RADIUS=RADIUS*1000.0
C
      NVERT=80
      CALL CIRCLE ( NVERT, XCEN, YCEN, RADIUS )
      CALL DRWSYM ( NVERT )
      call flsh
      write(*,*)char(27),char(12)
      GO TO 88888
C
C  GENERAL POLYGON CREATE
C
40000 CONTINUE
c*
c*
       if(idtype.eq.1) then
        write(nprnt,77001)
77001   format(' ENTER (CR), Map will redraw, Use cursor and space bar',
     +' to make POINT(S).')
c 
       else
c
       IF(DIGITZ) WRITE(NPRNT, 39999)
39999  format(' ENTER POINTS. HIT PAD KEY 9 TO TERMINATE'\)
       IF(.NOT.DIGITZ) WRITE(NPRNT, 40001)
40001  format(' ENTER POINTS FOR FEATURE. HIT SPACE-BAR FOR EACH'/ 
     * ' POINT.  TERMINATE WITH T KEY. CR=Continue '\)
c
      end if
c
       call readin(iget,1,ic,iend)
       if(iget.eq.ichar('Q')) go to 200
c*
c      LINES=LINES+2
      NVERT=0
c
      if(.not.key) then
        rewind(jchan)
        write(*,*) char(27),char(12),char(27),char(29)
       if(ido) call drfast(jchan,ier)
       if(items.gt.1)then
        rewind(70)
        call drfast(70,ier)
        backspace(70)
       endif
      endif
c
40003 CONTINUE
      IF(KEY) GO TO 40005
c**      IF(DIGITZ) CALL DIGZIN ( XT, YT, NT, ICHAND, IER )
c**      IF(DIGITZ) CALL DWRAP ( XT, YT, NCONT )
C      
         CALL VCURSR ( NT, XT, YT )
c
c  check both capital and small T
c
      IF(NT.EQ.84.or.nt.eq.116) GO TO 40002
      GO TO 40006
40005 CONTINUE
      WRITE(NPRNT,40007)
40007 FORMAT(' ENTER X,Y OR E,N COORDINATES '\)
c      LINES = LINES + 2
      CALL READIN ( icard, lenicd, ic, iend )
      if(icard(1).eq.ichar('T').or.icard(1).eq.ichar('t')) go to 40002
c**
      CALL FNUM ( ICARD, 80, XT, ic )
      CALL FNUM ( ICARD, 80, YT, ic )
40006 CONTINUE
      NVERT=NVERT+1
      call spoints('X ',x,xt,nvert,2)
      call spoints('Y ',y,yt,nvert,2)
c      X(NVERT)=XT
c      Y(NVERT)=YT
C
      if(idtype.gt.1)then 
      IF(NVERT.EQ.1) CALL MOVEA ( XT, YT )
      IF(NVERT.GT.1) CALL DRAWA ( XT, YT )
      CALL FLSH
      if(nvert.eq.1)then
       xxt=xt
       yyt=yt
      endif
      go to 40003
      else
       CALL XXX ( XT, YT )
       write(*,*)char(27),char(12)
       GO TO 88888
      endif
C
40002 CONTINUE
      IF(IDTYPE.NE.3) GO TO 88888
C
C make last+1 points = first
C
      NVERT=NVERT+1
      call spoints('X ',x,xxt,nvert,2)
      call spoints('Y ',y,yyt,nvert,2)
c      X(NVERT)=XT
c      Y(NVERT)=YT
      CALL DRAWA(X(NVERT),Y(NVERT))
      CALL FLSH
      write(*,*)char(27),char(12)
88888 CONTINUE
C
C  STORE DATA TYPE
C
      IARR(6)=IDTYPE
C
C  CALCULATE MBR
C
C*      CALL CALMBR ( NVERT, XMIN, XMAX, YMIN, YMAX )
        call calmbr ( nvert, xmin, xmax, ymin, ymax )
C
C  CALCULATE THE AREA IF IT IS A POLYGON
C
      IF(IDTYPE.EQ.3) CALL CENTAR(NVERT,XCENT,YCENT,XT,ISLAND,IPOINT,
     1                            IUNITS)
C
C  IF LINE, CALCULATE THE LENGTH
C
      IF(IDTYPE.EQ.2) XT=RLNLEN(NVERT)
C
C  STORE AREA OR LENGTH
C
      IARR(7)=IXT(1)
      IARR(8)=IXT(2)
C
C  IF POLYGON, CALCULATE THE PERIMETER
C
      IF(IDTYPE.NE.3) GO TO 41
      CALL PPERIM ( NVERT, ISLAND, IPOINT, XT )
      IARR(9)=IXT(1)
      IARR(10)=IXT(2)
C        IF(IDEB.GT.0) WRITE(NPRNT,2222) XT
C 2222 FORMAT(' PERIM = ',F10.2)
   41 CONTINUE
C
C  IF POLYGON, PLACE CENTROID IN HEADER
C
      IF(IDTYPE.NE.3) GO TO 42
      XT=XCENT
      YT=YCENT
      IARR(11)=IXT(1)
      IARR(12)=IXT(2)
      IARR(13)=IYT(1)
      IARR(14)=IYT(2)
   42 CONTINUE
C
C  KEEP TRACT OF MAP WINDOW
C
      XXMN=AMIN1(XXMN,XMIN)
      YYMN=AMIN1(YYMN,YMIN)
      XXMX=AMAX1(XXMX,XMAX)
      YYMX=AMAX1(YYMX,YMAX)
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
C  SET POINTER FOR ISLAND POINTER STORAGE IN MAIN HEADER
C
      KPOINT=27
C
C  DETERMINE NUMBER OF RECORDS REQUIRED TO STORE ISLAND POINTERS
C
      INC=1
C
C  NOW ADD LINK POINTER TO NEXT RECORD AND NUMBER OF RECORDS IN THIS RECORD
C
      NR=(((NVERT-1)/64)+1)+1
      IARR(1)=NREC+NR
      IARR(2)=NR
C
C  SET RECORD INCREMENT TO 1
C
      IF(NSLAND.EQ.0) GO TO 46
      DO 45 II=1,NSLAND
        IARR(KPOINT)=ISLAND(II)
        KPOINT=KPOINT+1
   45 CONTINUE
   46 CONTINUE
C
C  WRITE OUT FIXED LENGTH PORTION OF RECORD
C
      IARR(6)=IDTYPE
      CALL WRBLK ( ICHAN5, NREC, IARR, 1, IER )
C
C  STORE CURRENT RECORD POINTER FOR RECORD LINK CONSTRUCTION - INC RECORD #
C
      NNREC=NREC
      NREC=NREC+INC
C
C  SAVE INC IN IARR FOR USE IN COWRT
C
      IARR(127)=INC
C
C  STORE COORDINATE DATA
C
      CALL COWRT ( IARR, NVERT, NREC, XMIN, YMIN, ICHAN5, IZ )
C
C  ALL DONE
C
c      LINES=1
      GO TO 100
C
C  UPDATE ACTUAL SUBJECT AND CATEGORY COUNTERS
C
  200 CALL RDBLK ( ICHAN5, 1, IHEAD, 1, IER )
      write(*,*)char(27),char(12) 
      IHEAD(1) = NREC - 1
      IHEAD(62)=IDTYPE
      IHEAD(63)=ITEMS
      IHEAD(66)=NNCAT
      IHEAD(67)=NSUB
C
C  INSERT MAP MINS AND MAXS INTO HEADER
C
      IHEAD(68)=JXMIN(1)
      IHEAD(69)=JXMIN(2)
      IHEAD(70)=JXMAX(1)
      IHEAD(71)=JXMAX(2)
      IHEAD(72)=JYMIN(1)
      IHEAD(73)=JYMIN(2)
      IHEAD(74)=JYMAX(1)
      IHEAD(75)=JYMAX(2)
C
C  NOW WRITE HEADER BACK OUT
C
      CALL WRBLK ( ICHAN5, 1, IHEAD(  1), 1, IER )
      CALL RDBLK ( ICHAN5, 2, IHEAD(129), 1, IER )
C      CALL TUPDIR ( ITP1, ITP2, ITP3, LABEL, IHEAD, 'POLYGON.DH', 3,
C     1              IER )
C
      call tupdir ( itp1, itp2, itp3, label, ihead, iwork,
     *icwork, ier)
c*
C
C  ALL DONE
C
      GO TO 999
C
C  ERROR RETURNS
C
C  900 CALL CALERR(16,0,LINES,'*GENERATE*',10,ISUB)
C      GO TO 999
c  901 CALL CALERR(4, 0, LINES,'*GENERATE*',10,ISUB )
C      CALL DFILW(ISUB,IER)
C      GO TO 999
C  907 CALL CALERR(17,0,LINES,'*GENERATE*',10,ISUB)
C      GO TO 999
c*
  901 write(nprnt,902) (mapname(I),I=1,25)
  902 format(/,1x,'Error on accessing input map - ',25a1)
      go to 999
c
  909 WRITE(NPRNT,3009)
 3009 FORMAT(' *GENERATE*  INVALID WINDOW OR WINDOW NOT SET')
c      LINES = LINES + 2
      GO TO 999
c  911 WRITE(NPRNT,3011) MAP,IER
c 3011 FORMAT(' *GENERATE*  ERROR OPENNING MAP ',15A2,'ERROR',I6)
c      LINES = LINES + 2
c      GO TO 999
  915 WRITE(NPRNT,3015)
 3015 FORMAT(' *GENERATE* COULD NOT NOT OPEN DATA CHANNEL TO DISPLAY')
c      LINES=LINES+2
C
C  WAY OUT
C
  999 CONTINUE
C      IF ( DIGITZ ) CALL CLOSE ( ICHAND, IER )
C      IF ( DIGITZ ) CLOSE ( ICHAND,iostat=IER )
      CLOSE ( ICHAN5,iostat=ier )
      close ( icwork,iostat=ier )
      CALL OUTCM
      END
