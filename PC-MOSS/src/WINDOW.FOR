C ***************************************************
C
C             P R O G R A M    W I N D
C FUNCTION:
C             THIS ROUTINE RESETS THE VIEWING WINDOW.
C PARAMETERS:
C             IER       OUT - THE ERROR RETURN FOR WRITING TO, READING,
C                             OPENING, OR CLOSING A FILE,
C                             IER = 0, IF AN ERROR,
C                             IER = 1, IF NO ERROR.
C ROUTINES
C CALLED:     DECIP,DAGET,SETWND,FNUM,CLEAR,OUTCM,GTMAP,APGET,GTNAM,CPLAC,
C             RDBLK,READIN,GETWND,CLOSE.
C
C ***************************************************
C
$Include:'DBCHAN.inc'
$Include:'IO.inc'
$Include:'DNAMES.inc'
$Include:'STRNG.inc'
$Include:'VWND.inc'
$Include:'WORK.inc'
       common/plact/macts
       COMMON /TYPE/ IDATYP(40),NACTS
       COMMON /TABLE/ITABLE(40)
       COMMON /WHMAP/ISTRCT(300),NSTRCT
       INTEGER MBR(8),mapname(25)
       REAL IWIND(4)
       DIMENSION ITRN(2)
       LOGICAL MCOMP
       EQUIVALENCE (ITRN(1),XT)
       EQUIVALENCE (IWIND,MBR)
C
       ICMAP = 20
C
C initialize to current settings
C
      Call initl(ier)
      if(ier.ne.0)stop
C
      call openf(icdcfa,idesfa,1,512,2,ier)
C
C  SET CURSOR COUNTER
C
      LINES=1
C
C  CHECK FOR WINDOW USING DATA SET
C
       KP=ICARDP
       CALL DECIP(ICARD,ICOM,KP)
       IF(MCOMP(ICOM,'ME',2,IER)) GO TO 5
       GO TO 100
C
C  OPTION IS WINDOW 'METERS'...HAS USER ALREADY ENTERED WINDOW ?
C
 5     CONTINUE
      ICARDP=KP
      CALL FNUM(ICARD,80,XORGIN,ICARDP)
      IF(XORGIN.EQ.0.0) GO TO 10
      CALL FNUM(ICARD,80,YORGIN,ICARDP)
      IF(YORGIN.EQ.0.0) GO TO 20
      CALL FNUM(ICARD,80,XALEN,ICARDP)
      IF(XALEN.EQ.0.0) GO TO 30
      CALL FNUM(ICARD,80,YALEN,ICARDP)
      IF(YALEN.EQ.0.0) GO TO 40
      GO TO 50
C
C  PROMPT THE USER FOR THE DATA
C
  10  CONTINUE
      WRITE(NPRNT,1000)
 1000 FORMAT(22H ENTER SW X COORDINATE ,\)
      CALL READIN(ICARD,LENICD,ICARDP,istop)
      CALL FNUM(ICARD,80,XORGIN,ICARDP)
      LINES=LINES+3
  20  CONTINUE
      WRITE(NPRNT,1001)
 1001 FORMAT(22H ENTER SW Y COORDINATE  ,\)
      CALL READIN(ICARD,LENICD,ICARDP,istop)
      CALL FNUM(ICARD,80,YORGIN,ICARDP)
      LINES=LINES+3
  30  CONTINUE
      WRITE(NPRNT,1002)
 1002 FORMAT(17H ENTER X DISTANCE  ,\)
      CALL READIN(ICARD,LENICD,ICARDP,istop)
      CALL FNUM(ICARD,80,XALEN,ICARDP)
      LINES=LINES+3
  40  CONTINUE
      WRITE(NPRNT,1003)
 1003 FORMAT(17H ENTER Y DISTANCE  ,\)
      CALL READIN(ICARD,LENICD,ICARDP,istop)
      CALL FNUM(ICARD,80,YALEN,ICARDP)
      LINES=LINES+3
  50  CONTINUE
      IBW=0
      GO TO 210
C
C
C  SEE IF THER'S ANY ACTIVE MAPS...IF SO THEN
C  SET DISPLAY WINDOW USING MAPS...SKIP 'MAP' IN WINDOW MAP
C
C
100    IF (NACTS .LT. 1) GO TO 920
       IF(MCOMP(ICOM,'M',1,IER)) ICARDP=KP
C
C  GET MAPID'S OF MAPS TO USE
C
105    CALL DAGET(ICARD,ICARDP,ITABLE,40,MACTS)
       IF (MACTS .GT. 0) GO TO 110
C
C  PROMPT FOR MAPID'S
C
      WRITE(NPRNT,1004)
 1004 FORMAT(' ENTER ACTIVE MAP I.D.(S) TO WINDOW (CR=QUIT)'\)
      CALL READIN(ICARD,LENICD,ICARDP,istop)
      IF(ICARD(1).EQ.0) GOTO 90000
      GO TO 105
C
C  SET PARAMETERS
C
110   IBW=0
      XORGIN=99999999.
      YORGIN=XORGIN
      XMAX=-XORGIN
      YMAX=XMAX
C
C  ENTER MAIN LOOP
C
       do 161 ill=1,macts
C
C  GET MAP NAME & F.I.T. FOR THIS DATA SET
C
      IDD=(ITABLE(ill)-1)/7+1
      IOPT = 0
      IF(IDATYP(IDD) .GE. 6  .AND.  IDATYP(IDD) .LE. 9) IOPT = 5
C
C  OPEN DATA CHANNEL TO THIS MAP
C
      CALL GTNAM(ITABLE(ill),IOPT,2h  ,MAP,IER)
C
       call adddir(map,mapname)
C
      CALL OPENF(ICMAP,MAPname,1,256,2,IER)
      IF(IER.NE.0) GO TO 85020
C
C  GET POINTER BOUNDS
C
      CALL APGET(ITABLE(ill),ISTART,ISTOP,NTOTAL)
C
C  SEE IF THERE'S A SELECT ALL...AND IT'S NOT AN OLD MAP WITH 0 OR BLANK
C
       CALL RDBLK(ICMAP,1,IBUFF(1),1,IER)
       CLOSE(ICMAP,iostat=IER)
       IF (IBUFF(62) .LT. 1  .OR.  IBUFF(62) .GT. 13) GO TO 120
       IF (IBUFF(62) .GE. 6  .AND.  IBUFF(62) .LE. 9) GO TO 157
       IF (IBUFF(63) .EQ. NTOTAL) GO TO 157
C
C  ENTER CALCULATION LOOP FOR VECTOR MAPS
C
 120  CONTINUE
      DO 150 I=ISTART,ISTOP
C
C  GET POINTER
C
      CALL GETFAST(I,MBR,INDEX,ISUBN,IEX1,IEX2,IER)
C
C  REGEN WINDOW BOUNDS
C
      XT=IWIND(1)
      XORGIN=AMIN1(XORGIN,XT)
      YT=IWIND(3)
      YORGIN=AMIN1(YORGIN,YT)
      XT=IWIND(2)
      XMAX=AMAX1(XMAX,XT)
      YT=IWIND(4)
      YMAX=AMAX1(YMAX,YT)
 150  CONTINUE
      GO TO 160
C
C  WHOLE MAP WINDOW USING MAP MBR
C
 157  ITRN(1)=IBUFF(68)
      ITRN(2)=IBUFF(69)
      XORGIN=AMIN1(XORGIN,XT)
      ITRN(1)=IBUFF(70)
      ITRN(2)=IBUFF(71)
      XMAX=AMAX1(XMAX,XT)
      ITRN(1)=IBUFF(72)
      ITRN(2)=IBUFF(73)
      YORGIN=AMIN1(YORGIN,XT)
      ITRN(1)=IBUFF(74)
      ITRN(2)=IBUFF(75)
      YMAX=AMAX1(YMAX,XT)
C
160   close(icmap)
161   continue
C
C  GET LENGTH & WRITE OUT COMMON & RETURN
C
200   XALEN=XMAX-XORGIN
      YALEN=YMAX-YORGIN
 210  CONTINUE
      CALL SETWND
      CALL OUTCM
      GO TO 90000
C
C**     ERROR RETURNS
C
 920   write(nprnt,92001)
92001  format(/,1x,'Display window not set. Set window to',
     +  ' active maps(s).')
       GO TO 90000
C
85020  CONTINUE
       WRITE(NPRNT,98502) (MAPname(I),I=1,25),IER
98502  FORMAT(' *WIND*   ERROR FROM OPEN OF ',25A1,'  ERROR',I5)
       LINES = LINES + 2
       CLOSE(ifchan,iostat=IER)
       GO TO 90000
C
C**     JUMP OUT
C
90000  CONTINUE
       END
