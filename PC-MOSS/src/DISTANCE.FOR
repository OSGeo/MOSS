C******************************************************************************
C
C             P R O G R A M      D I S T A N C E
C
C FUNCTION:   DETERMINE THE DISTANCE BETWEEN ANY TWO USER
C             SPECIFIED POINTS ON THE SCREEN
C******************************************************************************
C
$Include:'dbchan.inc'
$Include:'xypnts.inc'
      COMMON /IO/     NPRNT,IOIN
      COMMON /FLS/    IUNIT,IFILE(5)
C      COMMON /RBX/    IRBGX,ICHX
C      COMMON /RBY/    IRBGY,ICHY
      COMMON /PLACT/  MACTS
      COMMON /TYPE/   IDATYP(40),NACTS
      COMMON /TABLE/  ITABLE(40)
$Include:'strng.inc'
      COMMON /WHMAP/  ISTRCT(300),NSTRCT
      COMMON /PNTFET/ NPOINT
      COMMON /WORK/   IBUFF(128),MAP(25),ICOM(10),JCOM(10)
      REAL LEN,IWIND(4),IX1,IY1,IX2,IY2
      integer mbr(8),mapname(25)
      LOGICAL FLINE,MCOMP
C
C      READ IN COMMON BLOCKS
C
        CALL initl(ier)
        if(ier.ne.0)stop
C
        CALL OPENF(icdcfa,idesfa,1,512,2,IER)
        IF(IER.NE.0) GO TO 901
C
C    ASSIGN CHANNEL NUMBERS FOR MAP FILE AND ARRAY FILES
C
        ICHAN=10
C
C   OPEN MAP FILE
C
        CALL GTNAM(ITABLE(1),0,2h  ,MAP,IER)
        call adddir(map,mapname)
        CALL OPENF(ICHAN,MAPname,1,256,2,IER)
        IF(IER.NE.0) GO TO 902
C        CALL OPENF(ICHX,'X',2,256,2,IER)
C        IF(IER.NE.0) GO TO 903
C        CALL OPENF(ICHY,'Y',2,256,2,IER)
C        IF(IER.NE.0) GO TO 903
C
C  GET COORDINATE TYPE AND SET CONVERSION FLAG
C
        CALL RDBLK(ICHAN,1,IBUFF,1,IER)
        ICONV=1
        IF(MCOMP(IBUFF(77),'F',1,IER)) ICONV=2
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
C  SET CURSOR COUNTER
C
      CALL SETWND
      LINES=1
C  GET DISTANCE TYPE
C
      CALL DECIP(ICARD,ICOM,K)
      IF(ICOM(1).NE.0) GO TO 15
C
C  PROMPT FOR TYPE
C
   20 WRITE(NPRNT,1003)
 1003 FORMAT(' Enter:  [A]irline, [P]ath, [Q]uit'\)
      CALL READIN(ICARD,lenicd,K,istop)
      CALL DECIP(ICARD,ICOM,K)
   15 IF((MCOMP(ICOM(1),'P',1,IER)) .OR.
     +   (MCOMP(ICOM(1),'A',1,IER))) GO TO 10
      IF((MCOMP(ICOM(1),'Q',1,IER))) GO TO 9000
      WRITE(NPRNT,1004)
 1004 FORMAT(' INCORRECT DIRECTIVE ENTERED -  TRY AGAIN')
      GO TO 20
C
  10  CONTINUE
C      CALL CPLAC(3)
C
C  INPUT COMPLETE. LETS CALCULATE DISTANCE BY JUMPING TO
C  PROPER SECTION OF ROUTINE
C  FIRST, HOWEVER, LETS GET THE INPUT POINTS FROM THE USER
C
C  GET THE TWO POINTS


C
      WRITE(NPRNT,1000)
 1000 FORMAT(' POINT TO TWO LOCATIONS - use a SP to continue ',/,
     + ' input for airline distance. Any other key will give ',
     + 'DISTANCE.',/,'  -( give a CR to proceed).'\)
C
      call readin(icard,lenicd,icardp,istop)
      rewind(jchan)
      write(*,*)char(27),char(12)
      call drfast(jchan,ier)
      CALL VCURSR(IT,X1,Y1)
      CALL XXX(X1,Y1)
C
      CALL VCURSR(IT,X2,Y2)
      CALL XXX(X2,Y2)
C
      IF(MCOMP(ICOM(1),'P',1,IER)) GO TO 25
C
C  DRAW LINE BETWEEN TWO POINTS
C
      CALL MOVEA(X1,Y1)
      CALL DRAWA(X2,Y2)
C
C  PUT CURSOR BACK WHERE IT BELONGS
C
  25  CONTINUE
C     
C  JUMP TO PROPER ACTION
C
      IF( MCOMP(ICOM(1),'P',1,IER)) GO TO 30
C
C  CALCULATE DISTANCE AND WRITE IT
C
      LEN=SQRT(((X2-X1)*(X2-X1)) + ((Y2-Y1)*(Y2-Y1)))
 400  CONTINUE
      X1=X2
      Y1=Y2
      CALL ANMODE
C      CALL POSTF
C      WRITE(NPRNT,403)
C  403 FORMAT(' CONTINUE Air with CR or SPACE - other keys report',
C     +' DISTANCE')
C      rewind(jchan)
C      call drfast(jchan,ier)
      CALL VCURSR(ICOM(1),X2,Y2)
      CALL XXX(X2,Y2)
C
      IF ( ICOM(1).NE.10 .AND. ICOM(1).NE.13
     + .AND. ICOM(1).NE.32 ) GO TO 402
C
      CALL MOVEA(X1,Y1)
      CALL DRAWA(X2,Y2)
C
      LEN=LEN + SQRT(((X2-X1)*(X2-X1)) + ((Y2-Y1)*(Y2-Y1)))
C
      GO TO 400
C
 402  CONTINUE
       write(*,*)char(27),char(12)
      CALL UDCONV(ICONV,1,LEN,DIST,IER)
      WRITE(NPRNT,1001) DIST
 1001 FORMAT(' DISTANCE = ',F10.4,' MILES'  )
      CALL UDCONV(ICONV,2,LEN,DIST,IER)
      WRITE(NPRNT,1002) DIST
 1002 FORMAT(12X ,F10.4,' KILOMETERS' )
      CALL UDCONV(ICONV,4,LEN,DIST,IER)
      WRITE(NPRNT,1020) DIST
 1020 FORMAT(12X,F10.1,' FEET')
      CALL UDCONV(ICONV,6,LEN,DIST,IER)
      WRITE(NPRNT,1021) DIST
 1021 FORMAT(12X,F10.2,' NAUTICAL MILES')
      LINES=LINES+5
      GO TO 20
C
C  DISTANCE ALONG A LINE
C
  30  write(*,*)char(27),char(12)
C
C  CHECK FOR BAD CURSOR INPUT
C
      IF(X2-X1.EQ.0.0.AND.Y2-Y1.EQ.0.0)GO TO 899
C
C  CALCULATE SEARCH TOLERANCE
C
      CALL WHWND(IX1,IY1,IX2,IY2)
C
C  DECREASE TOLERANCE TO ELIMINATE ISLAND JUMPPING (.../2.) - GWF 12-17-85
      ATOL=(AMAX1(IX2-IX1,IY2-IY1)/70.0)/2.
C
C  LOOP THROUGH FOR EACH MAP DISPLAYED ON THE SCREEN
C
      DO 100 II=1,MACTS
C
C  GET POINTER SET FOR THIS MAP
C
          INDEX=ITABLE(II)
          IF(INDEX.LE.0) GO TO 100
          CALL APGET(INDEX,ISTART,ISTOP,NTOTAL)
          IF(NTOTAL.LE.0) GO TO 100
C
C  OPEN MAP
C
           CLOSE(ICHAN,iostat=IER)
           CALL GTNAM(ITABLE(II),0,2h  ,MAP,IER)
           CALL OPENF(ICHAN,MAP,1,256,2,IER)
C
C  CHECK EACH ITEM OF THIS MAP (II)
C
          DO 50 I=ISTART,ISTOP
C
C  GET ITEM MININUM BOUNDING RECTANGLE
C
              CALL GETfast(I,MBR,INDEX,isubj,iattr,ixtra,IER)
              IF(IER.EQ.0) GO TO 35
              IF(IER.EQ.-1) WRITE(NPRNT,2007) I
2007          FORMAT('INVALID POINTER INDEX',I5)
              IF(IER.EQ.-2) WRITE(NPRNT,2008) INDEX
2008          FORMAT('INVALID POINTER VALUE',I5)
              GO TO 50
35            CALL GETWND(INDEX,IWIND,ICHAN)
              XMIN=IWIND(1)-ATOL
              XMAX=IWIND(2)+ATOL
              YMIN=IWIND(3)-ATOL
              YMAX=IWIND(4)+ATOL
C
C  DO THE USER INPUT POINTS FALL WITHIN THE MBR OF THIS ITEM ?
C
              IF(INVWND(X1,X1,Y1,Y1,XMIN,XMAX,YMIN,YMAX).EQ.0)GO TO 50
              IF(INVWND(X2,X2,Y2,Y2,XMIN,XMAX,YMIN,YMAX).EQ.0)GO TO 50
C
C  POINT IN MBR.  GET ITEM DATA
C
              CALL RDBLK(ICHAN,INDEX,IBUFF,1,IER)
C
C  GET THE (X,Y) COORDINATES
C
              CALL CORDXY(IBUFF,NPTS,INDEX,IWIND(1),IWIND(3),ICHAN)
C
C  IS THIS THE LINE WE WANT?
C
              IF(.NOT.FLINE(X1,Y1,NPTS,ATOL,IPTR1))GO TO 50
              IF(.NOT.FLINE(X2,Y2,NPTS,ATOL,IPTR2))GO TO 50
C
C  THIS IS THE LINE.  CALCALATE PATH DISTANCE
C
              GO TO 60
C
  50      CONTINUE
 100  CONTINUE
      LINES=LINES+2
      WRITE(NPRNT,2003)
 2003 FORMAT(' Input locations MUST NOT cross ITEM BOUNDS')
      GO TO 20
C
C  CALCULATE PATH DISTANCE
C
  60  CONTINUE
      NSEGS=IABS(IPTR1-IPTR2)
      IPT1=IPTR1
       IPT2=IPTR2
       IF(IPT1.LT.IPT2) GO TO 61
         TT=X1
         X1=X2
         X2=TT
         TT=Y1
         Y1=Y2
         Y2=TT
         IT=IPT1
         IPT1=IPT2
         IPT2=IT
  61   CONTINUE
C
C      IF ( X(IPT1+1).LT.0. ) GO TO 904
C
      call spoints('X ',x,xipt11,ipt1+1,1)
      call spoints('Y ',y,yipt11,ipt1+1,1)
      LEN=SQRT((X1-XIPT11)**2+(Y1-YIPT11)**2)
      IF(NSEGS.LE.1) GO TO 45
C
      IPT1=IPT1+1
      NSEGS=NSEGS-1
      DO 40 J=1,NSEGS
        call spoints('X ',x,xipt1,ipt1,1)
        call spoints('X ',x,xipt11,ipt1+1,1)
        call spoints('Y ',y,yipt1,ipt1,1)
        call spoints('Y ',y,yipt11,ipt1+1,1)
        LEN=LEN+SQRT((XIPT1-XIPT11)**2+(YIPT1-YIPT11)**2)
        IPT1=IPT1+1
        IF (IPT1 .GT. NPTS) IPT1 = 1
  40  CONTINUE
  45  CONTINUE
      call spoints('X ',x,xipt2,ipt2,1)
      call spoints('X ',y,yipt2,ipt2,1)
      LEN=LEN+SQRT((X2-XIPT2)**2+(Y2-YIPT2)**2)
C
C  PRINT OUT RESULT IN MILES,KILOMETERS, AND NAUTICAL MILES
C
      GO TO 402
C
C  ERROR RETURN
C
  899 WRITE(NPRNT,3001)
 3001 FORMAT(' BAD CURSOR INPUT. TRY AGAIN')
      LINES=LINES+2
      GO TO 20
C
C
  901 write(*,*)' Could not open <DESCRIBE.FA>'
      GO TO 9000
C
  902 write(*,*)' Could not open input map'
      GO TO 9000
C
  903  write(*,*)
C      GO TO 9000
C  904 WRITE(NPRNT,8904)
C 8904 FORMAT(' *DISTANCE*  ISLAND ENCOUNTERED IN PATH')
 9000  continue
      CLOSE(ICHAN,iostat=IER)
      CLOSE(icdcfa,iostat=IER)
       END
