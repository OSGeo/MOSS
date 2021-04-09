C******************************************************************************
C
C              P R O G R A M    L E G E N D
C FUNCTION:
C             THIS IS THE DRIVER FOR THE LEGEND COMMAND
C PARAMETERS:
C            IOIN  IN - INPUT
C            NPRNT IN - OUTPUT
C            IDEB  IN - DEBUG SWITCH
C            LINES IN - RETURNED CURSOR UPDATE INFO
C******************************************************************************
C
$Include:'mfast.inc'
$Include:'dbchan.inc'
        COMMON /UNIT/   ICHAN26,ATRBTE,POLYGON,ADRESS
$Include:'strng.inc'
         common/thic/dftlpn,ntim,pnwd
$Include:'xypnts.inc'
C$Include:'yryr.inc'
$Include:'pqpnts.inc'
$Include:'work.inc'
        COMMON /FLS/    IUNIT, IFILE(5)
        COMMON /IO/     NPRNT, IOIN
        COMMON /ATTFET/ KAT(86),LBAT
        COMMON /COMEXT/ KEYCON,FACX,FACY
        COMMON /TYPE/   IDATYP(40),NACTS
        COMMON /WHMAP/  ISTRCT(300),NSTRCT
        COMMON /TABLE/  ITABLE(40)
C
        INTEGER ATRBTE,POLYGON,ADRESS,IWIND(8),IDTYP,POINT,ICHAN
        integer isubj(60),mapname(25),isub(30)
        character*7 numb
        REAL WIND(4),DPORT(4),DVCTR(4),XMIN,YMIN,XMAX,YMAX
        REAL XI(2),YI(2),XJ(2),YJ(2)
        LOGICAL MCOMP,FOUND,TWICE,THRICE,CROWD,idone
        EQUIVALENCE (IBUFF(11),XTCENT),(IBUFF(13),YTCENT),
     +              (IWIND(1),WIND(1))
C
C   SET SOME VARIABLES
C
        IDEV  =  1
        ICHAN = 10
        ICHP  =  20
        LINES =  1
C
C**     OPEN FILES
C
        CALL initl(ier)
C
C
        CALL OPENF(icdcfa,idesfa,1,512,2,IER)
C
C   SET WINDOW FOR TEKTRONIX PLOTTING
C
C
C initilize display
C
      map(1)=2HLM
      map(2)=2h
      jchan=81
      call openf(jchan,map,1,0,1,ier)
      if(ier.ne.0)then
        write(nprnt,2)
   2  format(/,1x,'You must do the <PLOT> command first')
        return
      endif
C
      map(1)=2hZ
      call openf(70,map,3,0,1,ier)
C
      call initt(1,iunit,ifile)
C
        CALL SETWND
C
C  GET ACTIVE DATA SETS TO LEGEND
C
        CALL DAGET(ICARD,icardp,ITABLE,40,MACTS)
C
C  IF USER DID NOT SPECIFY ANYTHING, PROMPT THEM
C
      IF(MACTS.NE.0) GO TO 10
   5  WRITE(NPRNT,1005)
 1005 FORMAT(' ENTER ACTIVE MAP I.D. TO LEGEND'\)
      CALL READIN(ICARD,lenicd,icardp,istop)
      LINES=LINES+3
      CALL DAGET(ICARD,icardp,ITABLE,40,MACTS)
      IF (MACTS .LT. 1) GO TO 99999
      GO TO 15
C
C  GET LEGEND TYPE
C
  10  CALL DECIP(ICARD,ITYPE,icardp)
      IF(ITYPE.NE.0) GO TO 20
C
C  PROMPT FOR LEGEND TYPE
C
  15  CONTINUE
      IPROMT=1
      WRITE(NPRNT,1001)
 1001 FORMAT(' LABEL, DESCRIBE, OR CONTOUR ? '\)
      CALL READIN(ICARD,lenicd,icardp,istop)
      CALL DECIP(ICARD,ITYPE,icardp)
      LINES=LINES+3
C
C  JUMP TO APPROPRIATE ACTION
C
  20  CONTINUE

C
      IF(MCOMP(ITYPE,'D',1,IER)) GO TO 21
C      IF(MCOMP(ITYPE,'E',1,IER)) GO TO 10011
      IF(MCOMP(ITYPE,'L',1,IER)) GO TO 35
      IF(MCOMP(ITYPE,'C',1,IER)) GO TO 35
C
      WRITE(NPRNT,1002)
 1002 FORMAT(' WHAT?  PLEASE RE-ENTER.')
      GO TO 15
10011 write(*,*)'Elevation is not implemented in PC/MOSS'
      go to 15
C
C**     DESCRIBE(LEGEND)
C**
C
  21    WRITE( NPRNT, 70000 )
70000   FORMAT(' POINT TO THE LOCATION FOR THE TITLE AND SCALE',/
     +  ' - ENTER (CR) TO PROCEED.'\)
        call readin(iget,1,ic,iend)
        rewind(jchan)
        write(*,*)char(27),char(12)
        call drfast(jchan,ier)
        call flsh
        call toutpt(31)
        call toutpt(29)
C        CALL FLSH
        CALL VCURSR( I, X1, Y1 )
        CALL FLSH
C
C**  GET IT AND WRITE IT OUT
C
       IDD=ITABLE(1)
       CALL GTNAM(IDD,0,2h  ,jcom,IER)
C  preform a MOVE
        write(*,*)char(27),char(12)
        rewind(jchan)
        write(*,*)char(27),char(12)
        call drfast(jchan,ier)
        call flsh
        call toutpt(31)
        call toutpt(29)
       CALL tkPLOT(X1,Y1,3)
C**       PUT IN TEK ALPHA MODE & WRITE
            CALL TOUTPT(31)
C            CALL FLSH
       X1 = 1./FACX * 1023
       write(numb,'(f7.0)')x1
       call unpack(jcom,10)
       do 6 l=1,10
   6   call toutpt(jcom(l))
       call toutpt(32)
       call toutpt(ichar('S'))
       call toutpt(ichar('C'))
       call toutpt(ichar('A'))
       call toutpt(ichar('L'))
       call toutpt(ichar('E'))
       call toutpt(32)
       call toutpt(ichar('1'))
       call toutpt(ichar(':'))
       do 7 l=1,7
   7   if(ichar(numb(l:l)).gt.32)call toutpt(ichar(numb(l:l)))
C
        call flsh
        read(*,'(a1)') igoo
        write(*,*)char(27),char(12)
        CLOSE(ICHAN,iostat=IER)
        IF (IATYP .NE. 0) CLOSE(ICHAN26,iostat=IER)
        go to 99999
C  CHECK FOR SUBJECT DESIRED GIVEN MULTIPLE ATTRIBUTE SELECTION
C
   35 IATYP = 0
      idone=.FALSE.
      IF ( MCOMP(ICARD(icardp+1),'SU',2,IER) ) IATYP = -1
      IF ( IATYP.EQ.-1 ) CALL DECIP ( ICARD, ICOM, icardp )
C
C  **LABEL  OPTION...CHECK FOR 'HIDDEN' COUNT FOR OUTPUT STRING
C
       KK=icardp
       CALL INUM(ICARD,NCHARS,icardp)
       IF(NCHARS.LE.0) icardp=KK
       IF(NCHARS.LE.0) NCHARS=30
       IF(NCHARS.GT.30) NCHARS=30
C       NCHARS=(NCHARS-1)/2+1
1919    rewind(jchan)
        write(*,*)char(27),char(12)
        call drfast(jchan,ier)
        call flsh
        call toutpt(31)
        call toutpt(29)
         call toutpt(32)
C
C          WRITE(IUNIT,11111)
C11111     FORMAT(' ')
C
C  GET WINDOW AND CALCULATE LABELING CHARACTER HEIGHT
C
      CALL WHWND(XMIN,YMIN,XMAX,YMAX)
C......GET WINDOW AND ESTABLISH LABEL SIZE PARAMETER - HGT
C       CALL WHWND(X1,Y1,X2,Y2)
       TSCAL = AMAX1((XMAX-XMIN),(YMAX-YMIN))
       TSCAL = 10.14/(TSCAL)
       HGT = .13/TSCAL + .1
C
      DO 2000 III=1,MACTS
C        NCROWD = 0
        IPDX = 1
        IDD=ITABLE(III)
        CALL GTNAM(IDD,0,2h  ,MAP,IER)
C
C  OPEN DATA CHANNEL TO MAP
C
        call adddir(map,mapname)
        CALL OPENF(ICHAN,MAPname,1,256,2,IER)
        IF(IER.NE.0) GO TO 3040
C
C  CHECK FOR ATTRIBUTE INFO
C
        IF (IATYP.EQ.-1 ) GO TO 37
          IATYP=KAT((IDD/7)*2+1)
          MWORDS= ABS(IATYP)
          LOCATE=KAT((IDD/7)*2+2)
          CALL RDBLK(ICHAN,1,IBUFF,1,IER)
          IF (IBUFF(76) .NE. 1) IATYP = 0
C
C  ATTRIBUTE SELECTED MAP, BUT SUBJECT LEGEND DESIRED
   37   IF ( IATYP.EQ.-1 ) IATYP = 0
C
C  GET DATA SET POINTER BOUNDS FROM STRUCTURE TABLE
C
        CALL APGET(IDD,ISTART,ISTOP,NTOTAL)
C
C  LABEL ITEMS ON MAP
C  FIRST CHECK FOR ATTRIBUTE SELECTED DATA.
C
        IF(IATYP.EQ.0) GO TO 40
C
C  ATTRIBUTE SELECTED DATA. OPEN DATA CHANNEL TO PROPER FILE
C
        CALL GTNAM(IDD,4,2h  ,MAP,IER)
        ICHAN26=26
        call adddir(map,mapname)
        CALL OPENF(ICHAN26,MAPname,1,256,2,IER)
        IF(IER.NE.0) GO TO 3020
        IF(IATYP.LT.4) GO TO 40
        CALL INITIAL
        CALL RETRVL(FOUND,IBUFF,.FALSE.)
        IF(.NOT.FOUND) GO TO 99999
        IATYP=IBUFF(84)
        MWORDS=IBUFF(85)
        LOCATE=IBUFF(86)
C
C  LOAD THE WINDOW IN PREPARATION FOR PIP
C
  40    CONTINUE
        DPORT(1) = XMAX
        DPORT(2) = YMAX
        DPORT(3) = XMIN
        DPORT(4) = YMIN
C
C  ENTER LOOP
C
        DO 1000 II=ISTART,ISTOP
C
C  GET POINTER TO ITEM
C
          CALL GETFAST(II,IWIND,INDEX,IPOINT,IEX1,IEX2,IER)
          IF(IER.NE.0) GO TO 1000
C
C  CHECK TO SEE IF IN CURRENT WINDOW
C
          IF(INVWND(WIND(1),WIND(2),WIND(3),WIND(4),XMIN,XMAX,
     1              YMIN,YMAX).EQ.0) GO TO 1000
C
C  SOME PART IS SO GET FEATURE HEADER RECORD
C
          CALL RDBLK(ICHAN,INDEX,IBUFF,1,IER)
          IF (IER .NE. 0) GO TO 1000
C
C  SAVE SOME INFO AND PUT THE COORDINATE DATA IN DISK ARRAYS
C
          ITEM  = IBUFF(5)
          IDTYP = IBUFF(6)
          IF (IDTYP .EQ. 10  .OR. IDTYP .GT. 13) GO TO 3010
          IDTYP = MOD(IDTYP,10)
C
C  CHECK FOR A LINE MAP FOR CONTOURING
C
          IF(.NOT.MCOMP(ITYPE,'C',1,IER)) GO TO 175
             IF(IDTYP .EQ. 2) GO TO 174
             WRITE(NPRNT,1009)
 1009        FORMAT(' ONLY LINE MAPS CAN BE CONTOURED')
             GO TO 99999
C
C  CHECK TO OPEN THE FONTS FILE
C
  174     IF (idone) GO TO 175
      CALL plotsym(0,-61,0.0,0.0,0.0,0.0,5hfonts,0.0,0.0,0.0,0,5,IER)
          PENWID = .05
          pnwd=.05
          ntim=1
          idone=.TRUE.
C
  175     IPOINT= IBUFF(24)
          IVAL  = IBUFF(10)
          IF (IDTYP.EQ.3) XCENT=XTCENT
          IF (IDTYP.EQ.3) YCENT=YTCENT
C  ONLY USE COORDINATES OF PARENT POLYGON AND NOT ITS ISLANDS
          NVERT  = IBUFF(25)
          NVERT1 = NVERT
          IF (IDTYP .EQ. 3.AND.IBUFF(26).GT. 0) NVERT1 = IBUFF(27) - 1
          CALL CORDXY(IBUFF,NVERT,INDEX,WIND(1),WIND(3),ICHAN)
C
C  ESTABLISH A BEGINNING PLOT POINT
C

          IF(IDTYP.EQ.1.OR.IDTYP.EQ.5) call spoints('X ',x,xcent,1,1)
          IF(IDTYP.EQ.1.OR.IDTYP.EQ.5) call spoints('Y ',y,ycent,1,1)
          IF(IDTYP.EQ.2)call spoints('X ',x,xcent,(nvert1/2),1)
          IF(IDTYP.EQ.2) call spoints('Y ',y,ycent,(nvert1/2),1)
C
C  POINTS ALWAYS LABELS OK IF IN WINDOW AT ALL
C
          IF (IDTYP .EQ. 1  .OR.  IDTYP .EQ. 5) GO TO 500
C
C  LINE LABELS OK WHEN TOTALLY IN WINDOW
C
          IF (IDTYP .EQ. 2  .AND.
     +       (WIND(1) .GE. XMIN  .AND.  WIND(2) .LE. XMAX  .AND.
     +        WIND(3) .GE. YMIN  .AND.  WIND(4) .LE. YMAX) ) GO TO 500
C
C  SEE IF ANY POINT IN THE FEATURE IS INSIDE THE MBR OF THE POLYGON
C
           DO 280 KK=1,NVERT1-1
             call spoints('X ',x,dvctr(1),kk,1)
             call spoints('Y ',y,dvctr(2),kk,1)
             call spoints('X ',x,dvctr(3),kk+1,1)
             call spoints('Y ',y,dvctr(4),kk+1,1)
c             DVCTR(1) = X(KK)
c             DVCTR(2) = Y(KK)
c             DVCTR(3) = X(KK+1)
c             DVCTR(4) = Y(KK+1)
             CALL CLIP(DPORT,DVCTR,IPIP)
C  IF ANY POINT INSIDE THEN QUIT PROCESSING
             IF (IPIP .GE. 0) GO TO 400
 280        CONTINUE
C
C  NO POINTS INSIDE...SEE IF WINDOW INSIDE POLYGON
C
            IF (XMIN .GE. WIND(1)  .AND.  XMAX .LE. WIND(2)  .AND.
     +          YMIN .GE. WIND(3)  .AND.  YMAX .LE. WIND(4)) GO TO 400
            GO TO 1000
C
C  ENSURE THE LABEL IS ON THE LINE IF IT ISN'T TOTALLY IN AND THAT IT IS
C  SOMEWHERE IN THE POLYGON
C
 400    CONTINUE
          CALL LABMOV(WIND,NVERT1,IDTYP,XCENT,YCENT,IER)
C
C  JUMP TO ELEVATION PRINT
C
 500      CONTINUE
          IF(MCOMP(ITYPE,'E',1,IER)) GO TO 600
C
C  GET REGULAR LABEL OR MULTIPLE ATTRIBUTE LABEL
C
          CALL CLEAR ( MAP, 25 )
          IF(IATYP.EQ.0.AND.IDTYP.NE.5)
     +      CALL GETSUB(ICHAN,IPOINT,IBUFF,isub,I)
          IF(IATYP.NE.0) CALL GETAT ( ICHAN26, IATYP,
c error: not ISUBJ     +      MWORDS,LOCATE,IBUFF,ITEM,isubj,RNUM,IER)
     +    mwords,locate,ibuff,item,ISUB,rnum,ier)
       do 81 il=1,30
81     isubj(il)=isub(il)
C
C  JUMP TO CONTOUR PRINT
C
      IF (MCOMP(ITYPE,'C',1,IER)) GO TO 800
C
C  PLOT THE LABEL
C
          CALL TKPLOT(XCENT,YCENT,3)
C  PUT IN TEK ALPHA MODE & WRITE
            CALL TOUTPT(31)
C            CALL FLSH
           call unpack(isubj,60)
          if(idtyp.ne.5)then
            do 8 l=1,nchars
   8        call toutpt(isubj(l))
            else
            write(numb,'(i4)')ival
            do 9 l=1,4
   9        if(ichar(numb(l:l)).gt.32)call toutpt(ichar(numb(L:L)))
           endif
C          IF(IDTYP.NE.5) WRITE(IUNIT,1010) (MAP(I),I=1,NCHARS)
C1010      FORMAT(1X,15A2)
C          IF(IDTYP.EQ.5) WRITE(IUNIT,1011) IVAL
C1011      FORMAT(I4)
          GO TO 1000
C
C  PLOT ELEVATION POINTS
C
 600      CONTINUE
          DO 700 I=1,NVERT1
            call spoints('X ',x,xt,i,1)
            call spoints('Y ',y,yt,i,1)
c            call spoints('YR',yr,zt,i,1)
c            XT=X(I)
c            YT=Y(I)
c            ZT=YR(I)
            IVAL=ZT
            CALL TKPLOT(XT,YT,3)
C  PUT IN TEK ALPHA MODE & WRITE
            CALL TOUTPT(31)
            write(numb,'(i4)')ival
            do 11 l=1,4
   11       if(ichar(numb(L:L)).gt.32)call toutpt(ichar(numb(l:l)))
C
C              WRITE(IUNIT,1011) IVAL
  700     CONTINUE
          GO TO 1000
C
C**  PLOT CONTOUR LABELS EITHER AT THE MIDPOINT OR THIRD POINTS OF FEATURES
C
  800  CONTINUE
       TWICE  = .FALSE.
       THRICE = .FALSE.
C......IF FEATURE HAS MORE THAN 50 VERTICES, LABEL TWICE
       IF ( NVERT.GT.50  ) TWICE  = .TRUE.
       IF ( NVERT.GT.100 ) THRICE = .TRUE.
C......USE MIDPOINT IF LESS THAN 50 VERTICES
C......MORE THAN 50 VERTICES, FIRST ONE THIRD WAY DOWN VERTICES
C......MORE THAN 100 VERTICES, ONE QUARTER WAY DOWN VERTICES
       MID = NVERT/2
       IF ( TWICE  ) MID = NVERT / 3
       IF ( THRICE ) MID = NVERT / 4
       GO TO 830
C........SECOND TIME, TWO THIRDS WAY DOWN VERTICES
C........MORE THAN 100 VERTICES, HALF WAY DOWN VERTICES
  810    IF ( TWICE  ) MID = ( NVERT / 3 ) * 2
         IF ( THRICE ) MID = NVERT / 2
         TWICE = .FALSE.
         GO TO 830
C..........THIRD TIME THRU, THREE QUARTER WAY DOWN VERTICES
  820      MID = ( NVERT / 4 ) * 3
           THRICE = .FALSE.
C
  830  CONTINUE
C......SET START POSITION (XI(1),YI(1)) AND ROTATION DIRECTION (XI(2),YI(2))
       if(mid.eq.1)mid=2
       call spoints('X ',x,xi(1),mid-1,1)
       call spoints('Y ',y,yi(1),mid-1,1)
       call spoints('X ',x,xi(2),mid+1,1)
       call spoints('Y ',y,yi(2),mid+1,1)
c       XI(1) = X(MID-1)
c       YI(1) = Y(MID-1)
c       XI(2) = X(MID+1)
c       YI(2) = Y(MID+1)
C......FIND FIRST PRINTING CHARACTER OF LABEL
        call unpack(isubj,60)
        do 12 l=1,30
        if(isubj(l).gt.32)go to 16
   12   continue
C......FIND LAST CHARACTER
   16  ion=l
       do 13 l=25,1,-1
       if(isubj(l).gt.32)go to 14
   13  continue
C......LEFT JUSTIFY LABEL AND BLANK OUT RESIDUAL CHARACTERS
   14  nchars=l
       N = 0
       DO 835 M = ION, NCHARS+ION-1
         N = N + 1
         isubj(N) = isubj(M)
  835  CONTINUE
C......CORRECT LAST CHARATER POINTER
       call packc(isubj,isub,60)
       NCHARS = NCHARS - ION + 1
C
C........KEEP TRACK OF WHERE LABEL IS GOING TO BE PLACED
c                  p(ipdx)=xi(1)
          call spoints('P ',p,xi(1),ipdx,2)
c                  p(ipdx+1)=yi(1)
          call spoints('P ',p,yi(1),ipdx+1,2)
c                  p(ipdx+2)=xi(2)
          call spoints('P ',p,xi(2),ipdx+2,2)
C                  p(ipdx+3)=yi(2)
          call spoints('P ',p,yi(2),ipdx+3,2)
         IPDX = IPDX + 4
C........CALCULATE TOLERANCE FOR LABEL INTERFERENCE
         TOLR = HGT * 1.7
C........IF 2ND THRU LAST, CHECK OVER WRITING PREVIOUS LABEL
         CROWD = .FALSE.
         IF ( (IPDX-5)/4.LE.0 ) GO TO 850
           DO 825 M = 1, (IPDX-5)/4, 4
             IF ( CROWD ) GO TO 850
               call spoints('P ',p,xj(1),m,1)
               call spoints('P ',p,yj(1),m+1,1)
               call spoints('P ',p,xj(2),m+2,1)
               call spoints('P ',p,yj(2),m+3,1)
c               XJ(1) = P(M)
c               YJ(1) = P(M+1)
c               XJ(2) = P(M+2)
c               YJ(2) = P(M+3)
               CALL SINTER ( XI, YI, XJ, YJ, XINT, YINT, INTER, TOLR )
               IF ( INTER.EQ.1 .OR. INTER.EQ.2 ) CROWD = .TRUE.
  825      CONTINUE
  850   CONTINUE
C......PLACE LABEL ON CONTOUR IF NOT CROWDED
C         IF ( CROWD ) NCROWD = NCROWD + 1
C         IF ( CROWD ) CALL XXX ( XI(1), YI(1) )
C         IF ( CROWD ) CALL XXX ( XI(2), YI(2) )
C         CALL ANMODE
C         WRITE(NPRNT,855) II,MID,TOLR,NCROWD,M
C  855    FORMAT(' FEA: ',I5,'  MID:',I3,'  TOL:',F6.1,'  NCROWD:',I3,I4)
          if(.not.crowd) CALL CALLAB(HGT,isub,NCHARS,NVERT,
     +                                XI(1),YI(1),XI(2),YI(2))
C         IF ( CROWD ) PAUSE
       IF ( TWICE  ) GO TO 810
       IF ( THRICE ) GO TO 820
C
 1000   CONTINUE
9898    call flsh
        read(*,'(a1)') igoo
        write(*,*)char(27),char(12)
        CLOSE(ICHAN,iostat=IER)
        IF (IATYP .NE. 0) CLOSE(ICHAN26,iostat=IER)
 2000  CONTINUE
        IF (.NOT. MCOMP(ITYPE,'C',1,IER)) GO TO 99999
      CALL plotsym(0,-999,0.0,0.0,0.0,0.0,5hfonts,0.0,0.0,0.0,0,5,IER)
        GO TO 99999
C
C  ERROR RETURNS
C
 3000 write(*,*)' Could not properly open the pointer file'
      GO TO 99999
 3010 write(*,*)' This command does not work with raster data'
      GO TO 99999
 3020 write(*,*)' Could not open (.AT) file'
      GO TO 99999
 3040 write(*,*)' Error on accessing input map'
      GO TO 99999
C
C**     JUMP OUT
C
99999 CONTINUE
      END
