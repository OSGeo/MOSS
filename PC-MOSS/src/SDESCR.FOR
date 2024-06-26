      SUBROUTINE SDESCR (LINES,IDD)
      COMMON /STRNG/  ICARD(80), ICARDP, LENICD
      COMMON /TYPE/   IDATYP(40),MACTS
      COMMON /IO/     NPRNT, IOIN
      COMMON /ATTFET/ KAT(86),LBAT
      COMMON /UNIT/   CHANEL,ATRBTE,POLYGON,ADRESS
      DIMENSION IBUFF(128),MAP(25),IDESC(1000),POLSIZ(500),IDTYP(5,5)
      INTEGER CHANEL,POLYGON,ATRBTE,ADRESS,IMBR(8),mapname(25)
      LOGICAL FOUND
      EQUIVALENCE  (X1,IBUFF(15)), (X2,IBUFF(17)),
     +             (Y1,IBUFF(19)), (Y2,IBUFF(21)),
     +             (AREA,IBUFF(7)), (XLEN,IBUFF(9))
      DATA IDTYP / 2*2H  ,2H P,2HOI,2HNT,
     +             3*2H  ,2HLI,2HNE,
     +             2H  ,2H P,2HOL,2HYG,2HON,
     +             3*2H  ,2H A,2HRC,
     +             2H E,2HLE,2HVA,2HTI,2HON/
      DATA IBLNK /2H  /
C
C     ****************************************************
C
C                S U B R O U T I N E   S D E S C R
C
C     FUNCTION - CALCULATE THE DESCRIPTIVE STATISTICS FOR
C             THE AREA OR LENGTH VARIABLES OR A INTEGER OR REAL MULTIPLE
C             ATTRIBUTE FOR A SET OF FEATURES IN ONE ACTIVE DATA SET
C
C     INPUT PARAMETERS:
C      LINES   - CURSOR UPDATE VARIABLE
C      IDD     - ACTIVE MAP ID
C     ********************************************************
C
C
C   OPEN CHANNEL TO MAP DATA
C
      ICHAN = 15
      call gtnam(IDD,0,IBLNK,MAP,IER)
      call adddir(map,mapname)
      call openf(ICHAN,MAPname,1,256,2,IER)
      IF(IER.NE.0) GO TO 85020
C
C     GET DATA SET POINTER BOUNDS
C
      call apget (IDD,ISTART,ISTOP,NTOTAL)
C
C  READ HEADER AND DETERMINE IN MAP HAS MULTIPLE ATTRIBUTES
C
      call rdblk(ICHAN,1,IBUFF,1,IER)
      IF (IER .NE. 0) GO TO 90000
C
      ITYP = IBUFF(62)
      ITEMS= IBUFF(63)
C
C  SEE IF MULTIPLE ATTRIBUTES AND OPEN
C
      IF (IBUFF(76) .NE. 1) GO TO 80
      IF (KAT((IDD/7)*2+1) .EQ. 0) GO TO 80
C
C  GET THE TYPE, WORDS, LOCATE FROM ACTIVE SELECT
C
      IATYP = KAT((IDD/7)*2+1)
      MWORDS = IABS(IATYP)
      IF (IATYP .LT. 0) IATYP = 3
      LOCATE = KAT((IDD/7)*2+2)
C
C  OPEN MA FILE
C
      CHANEL = 26
      call gtnam(IDD,4,IBLNK,MAP,IER)
      call openf(CHANEL,MAP,1,256,2,IER)
      IF (IER .NE. 0) GO TO 85020
      call gtnam(IDD,0,IBLNK,MAP,IER)
C
C  USE THE SELECTED FIELD UNLESS THEY HAVE BSEARCH  - TYPE = 4
C
      IF (IATYP .GT. 3) GO TO 60
C
C  FIND THE SPECIFIED ATTRIBUTE TO WRITE OUT THE KEY & DESCRIPTION
C
       call rdblk(CHANEL,1,IDESC,1,IER)
       IATTS = IDESC(1)
       DO 40 I=2,IATTS+1
         call rdblk(CHANEL,I,IDESC,1,IER)
         IF (IDESC(85) .EQ. LOCATE) GO TO 70
  40   CONTINUE
       GO TO 70
C
C  SEE IF THEY NAME A FIELD
C
  60  call initial
      CALL RETRVL(FOUND,IDESC,.FALSE.)
      IF (.NOT. FOUND) CLOSE(CHANEL)
      IF (.NOT. FOUND) GO TO 80
C
C  DO THE MULTIPLE ATTRIBUTES BRAND OF DESCRIBE
C
      IATYP  = IDESC(84)
      MWORDS = IDESC(85)
      LOCATE = IDESC(86)
C
  70  CONTINUE
      IF (IATYP .NE. 1  .AND.  IATYP .NE. 2) GO TO 80000
      GO TO 1000
C
C    DETERMINE DATA TYPE - PRIOR TO CALLING UNSUB
C        JTYPE = 2  FOR POLYGON DATA
C        JTYPE = 3  FOR POINT DATA
C        JTYPE = 4  FOR LINE, ARC, OR ELEVATION DATA
C
  80  CONTINUE
      ITYPE = IDATYP(IDD/7 + 1)
      JTYPE = 4
      IF(ITYPE.EQ.3) JTYPE = 2
      IF(ITYPE.EQ.1) go to 908
C
C     GET TABULATIONS
C
      CALL UNSUB(JTYPE,ISTART,ISTOP,IDESC,1000,NSUBS,POLSIZ,500,ICHAN)
C
C     ENTER STATISTICS COMPUTATION LOOP, ONE ITERATION FOR
C     EACH UNIQUE SUBJECT  (NSUBS=NUMBER OF SUBJECTS)
C
      K1 = 1
      DO 130 II = 1,NSUBS
C
C     PRINT BASIC HEADER INFO
C
         IPOINT=IDESC(K1)
         CALL GETSUB(ICHAN,IPOINT,IBUFF,MAP,ITEMS)
         ITEMS=IDESC(K1+1)
         WRITE (NPRNT,150) MAP
         WRITE (NPRNT,160) ITEMS,NTOTAL
         LINES = LINES + 6
         IF (ITEMS.GT.3) GO TO 100
         WRITE(NPRNT, 170)
         GO TO 120
C
C     WE NOW HAVE TO LOOP THROUGH THE DATA AGAIN TO CALCULATE
C     THE RANGE, MIN, MAX, VARIANCE, AND STANDARD DEVIATION
C     FOR EACH UNIQUE SUBJECT
C
C     FIRST, SET SOME VARIABLES
C
 100     SSQ = 0.0
         XMIN = 9999999.
         XMAX = -(XMIN)
         XSUM = 0.0
         TAR = POLSIZ(II)
         XMEAN = TAR/FLOAT(ITEMS)
C
C     ENTER LOOP
C
         DO 110 JJ = ISTART,ISTOP
C
C     GET FIXED LENGTH PORTION OF A RECORD
C
C            call getndx(JJ,INDEX,IER)
            CALL GETFAST(JJ,IMBR,INDEX,ISUBJ,IATT,IEXT,IER)
            IF(IER.NE.0) GO TO 110
            call rdblk (ICHAN,INDEX,IBUFF,1,IER)
            IF (IER .NE. 0) GO TO 90000
            ITYP=IBUFF(6)
C
C     CORRECT SUBJECT FOR THIS PASS ?
C
            IPOINT=IBUFF(24)
            IF (IDESC(K1).NE.IPOINT) GO TO 110
C
C     WE ARE ALL SET.  UPDATE STATISTICS PARAMETERS
C
            IF (ITYP.EQ.3) TAR = AREA
            IF (ITYP.EQ.2.OR.ITYP.EQ.4) TAR = XLEN
            IF (TAR.LE.0.0) GO TO 110
            SSQ = SSQ + (TAR - XMEAN) *  * 2
            XMIN = AMIN1(XMIN,TAR)
            XMAX = AMAX1(XMAX,TAR)
            XSUM = XSUM + TAR
C
  110    CONTINUE
C
C     PRINT OUT THE REST OF THE TABLE FNOR THIS SUBJECT
C
         WRITE (NPRNT,180) XMIN,XMAX
C
         RANGE = XMAX - XMIN
         WRITE (NPRNT,190) RANGE,XMEAN
         VAR = SSQ/ITEMS
         SDEV = SQRT(VAR)
         WRITE (NPRNT,200) SDEV,VAR
         LINES = LINES + 2
         IF (ITYP.LE.0.OR.ITYP.GE.6) GO TO 120
         WRITE (NPRNT,210) XSUM,(IDTYP(I,ITYP),I=1,5)
         LINES = LINES + 1
C
C  UPDATE INDEX TO NSUB ARRAY
C
  120   K1=K1+2
        CALL CONTIN(LINES,IQUIT)
        IF (IQUIT .EQ. 1) GO TO 135
C
C     AND DO NEXT SUBJECT
C
  130 CONTINUE
  135 CONTINUE
C
      GO TO 90000
C
C  DO THE MULTIPLE ATTRIBUTES PORTION OF DESCRIBE
C
 1000  SSQ = 0.0
       XMIN = 9999999.
       XMAX = -(XMIN)
       XSUM = 0.0
C
C  LOOP TO GET THE MIN,MAX,SUM AND MEAN THEN LOOP TO GET THE SUM OF SQUARES
C
       DO 1180 I = 1,2
       DO 1160 JJ = ISTART,ISTOP
C         call getndx(JJ,INDEX,IER)
         CALL GETFAST(JJ,IMBR,INDEX,ISUBJ,IATT,IEXT,IER)
         call rdblk(ICHAN,INDEX,IBUFF,1,IER)
         IF (IER .NE. 0) GO TO 90000
C
C  GET THE ATTRIBUTE FIELD FOR THIS FEATURE
C
         IPOINT = IBUFF(5)
         CALL GETAT(CHANEL,IATYP,MWORDS,LOCATE,IBUFF,IPOINT,MAP,TAR,
     +              IER)
         IF (IER .NE. 0) GO TO 80020
C
C  UPDATE STATISTICS PARAMETERS
C
         IF (I .EQ. 1) XMIN = AMIN1(XMIN,TAR)
         IF (I .EQ. 1) XMAX = AMAX1(XMAX,TAR)
         IF (I .EQ. 1) XSUM = XSUM + TAR
         IF (I .EQ. 2) SSQ  = SSQ + ((TAR - XMEAN) * * 2)
C
 1160  CONTINUE
       XMEAN = XSUM / NTOTAL
 1180  CONTINUE
C
       CLOSE(CHANEL)
C
C  GET NAME AND COMPUTE THE STATS THEN WRITE THEM OUT
C
       call gtnam(IDD,0,IBLNK,MAP,IER)
       RANGE = XMAX - XMIN
       VAR   = SSQ / NTOTAL
       SDEV  = SQRT(VAR)
C
       WRITE(NPRNT,150) (MAP(I),I=1,25)
       CALL PACKC(IDESC(2),IBUFF,60)
       WRITE(NPRNT,155) IDESC(1),(IBUFF(I),I=1,30)
155    FORMAT(' ATTRIBUTE',I4,1X,5A2,1X,25A2)
       WRITE(NPRNT,160) NTOTAL,ITEMS
       WRITE(NPRNT,180) XMIN,XMAX
       WRITE(NPRNT,190) RANGE,XMEAN
       WRITE(NPRNT,200) SDEV,VAR
       WRITE(NPRNT,210) XSUM,(IDTYP(I,ITYP),I=1,5)
       LINES = LINES + 9
       GO TO 90000
C
C  FORMATS
C
  150    FORMAT (//29H DESCRIPTIVE STATISTICS FOR  ,25A2)
  160    FORMAT (/15H TOTAL CASES = ,I10,15H OUT OF      = ,I10)
  170    FORMAT (18H LESS THAN 3 CASES)
  180    FORMAT (15H MIN VALUE   = ,F10.1,15H MAX VALUE   = ,F10.1)
  190    FORMAT (15H RANGE       = ,F10.1,15H MEAN        = ,F10.1)
  200    FORMAT (15H STAN. DEV.  = ,F10.1,15H VARIANCE    = ,E14.7)
  210    FORMAT (15H TOTAL         ,F10.0,15H DATA TYPE     ,5A2)
C
C  ERROR RETURNS
C
80000 WRITE (NPRNT,98000) IATYP
98000 FORMAT(' *SDESCR*  CAN ONLY USE INTEGER AND REAL ATTRIBUTES ',
     +       'NOT TYPE',I5)
      LINES = LINES + 2
      CLOSE(CHANEL)
      GO TO 90000
C
80020 CONTINUE
      WRITE(NPRNT,98002) IER
98002 FORMAT(' *SDESCR*  ERROR FROM GETAT  ERROR',I5)
      LINES = LINES + 2
      CLOSE(CHANEL)
      GO TO 90000
C
 908  write(*,*)' Point maps can not be DESCRIBED'
      go to 90000
C
85020 WRITE (NPRNT,98502) (MAP(I),I=1,15),IER
98502 FORMAT(' *SDESCR*  ERROR FROM OPEN OF ',15A2,'  ERROR',I5)
      LINES = LINES + 2
C
C  JUMP OUT
C
90000 CONTINUE
      CLOSE(ICHAN)
      RETURN
      END
