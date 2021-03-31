       SUBROUTINE HSTCNT (IDD,ier)
C********************************************************************
C
C       ROUTINE SUBORDINATE TO THE MOSS STATISTICS COMMAND, HANDELS
C               HISTOGRAM LOGIC FOR CONTINUOUS CELL MAPS
C
C
C       IDD = STRUCTURE TABLE I.D. OF ACTIVE MAP OF INTEREST
c
C ********************************************************************
C
       COMMON /IO/ NPRNT, IOIN
C
       INTEGER IBUFF(512)
       INTEGER PLOTPRM(100),NUMB(200)
       INTEGER IVALUE(2)
       INTEGER MAP(25),mapname(25)
       INTEGER IER
       INTEGER TITLE(30)
C
       REAL RVALUE
       REAL BARBRK(100,2)
       REAL BARFRQ(100)
       REAL PLTWORK(100)
C
       LOGICAL OK,CHANGE
C
       EQUIVALENCE (RVALUE,IVALUE(1))
C
C*********************************************************************
C
       ICHAN = 15
       ier=0
       IDEVICE = 1
       IBLNK = 8224
C
C  OPEN .HD FILE
 1     call gtnam(IDD,5,IBLNK,MAP,IER)
       call adddir(map,mapname)
       CALL DSOPN(ICHAN,MAPname,1,1,1,256,256,IER)
       IF (IER .NE. 0) GO TO 8010
       IRECL = 512
       IV4 = 1
       CALL DSIN(ICHAN,IBUFF,1.0,IER)
C
C  PARSE OUT INFO
       MAPTYPE = IBUFF(62)
       NROWS = IBUFF(60)
       NCOLS = IBUFF(61)
       NSUBS = 100
       NATRBYT = IBUFF(191)/2
       CALL GTB(NBITS,IBUFF(185),1)
       ROWLEN = NBITS * NCOLS
C
C  SET INITIAL VALUES
       NWORDS = 1
       IF ( NBITS .GT. 16 ) NWORDS = 2
C
C  INITIATE AND FILL FREQUENCY ARRAY
       DO 100 I=1,100
          BARFRQ(I) = 0.0
100    CONTINUE
C
       PLOTPRM(1) = NSUBS
       PLOTPRM(2) = IBUFF(62)
C  CELL MIN & MAX VALUES (3-6)
       PLOTPRM(3) = IBUFF(88)
       PLOTPRM(4) = IBUFF(89)
       PLOTPRM(5) = IBUFF(90)
       PLOTPRM(6) = IBUFF(91)
C  PUT MAP DESCRIPTION IN TITLE ARRAY
       DO 200 I=1,30
          TITLE(I) = IBUFF(I+19)
200    CONTINUE
       len=30
       CALL LASTC(TITLE,LEN)
       PLOTPRM(11) = LEN
       DO 300 I=1,LEN+1/2
          PLOTPRM(I+11) = TITLE(I)
300    CONTINUE
C
C  CLOSE .HD FILE
       CALL DSCLS(ICHAN,IER)
C
C  SET UP PARAMETERS FOR CONTINUOUS HISTOGRAM
       OK = .TRUE.
       CALL CELPRM(PLOTPRM,BARBRK,CHANGE,OK)
       IF ( OK ) GO TO 400
          WRITE(NPRNT,1111)
1111      FORMAT(' ERROR COLLECTING PARAMETERS, PLEASE RE-ENTER')
          GO TO 1
C
400    NSUBS = PLOTPRM(1)
C
C  OPEN .CC FILE
       call gtnam(IDD,8,IBLNK,MAP,IER)
       CALL DSOPN(ICHAN,MAP,1,1,1,2,128,IER)
       IF (IER .NE. 0) GO TO 8010
       IRECL = 1024
C  CLEAR OUT ARRAYS
450    DO 500 I=1,512
          IBUFF(I) = 0
500    CONTINUE
C
       RECLST = 0.0
C
C  SEQUENCE THRU RECORDS
       RIV4 = 0
       DO 700 IROW = 1,NROWS
          BITPTR = (FLOAT(IROW-1) * ROWLEN)
          WRDPTR = AINT((BITPTR/FLOAT(16))) + 1.0
          RECPTR = AINT((WRDPTR - 1.0)/FLOAT(512)) + 1.0
C
C  IF NEW RECORD READ IN ROW
C          IF ( RECPTR .EQ. RECLST ) GO TO 530
C         IV4 = INT(RECPTR)
C             CALL DSIN (ICHAN, IBUFF(1), IV4, IRECL, IER)
C             IF (IER .NE. 0) GO TO 8020
c             RECLST = RECPTR
c             IDXWORD = 1
530       CONTINUE
C
C  PROCESS EACH CELL
          DO 600 ICOL = 1,NCOLS
C
C  CHECK IF WORD IS IN THIS RECORD
c             IF (FLOAT(IDXWORD) .LE. 512) GO TO 550
C
C  RESET WORD POINTER
                IDXWORD = 1
                RECPTR = RECPTR + 1.0
                RECLST = RECPTR
C
C  READ NEXT PHYSICAL RECORD
C         IV4 = INT(RECPTR)
          RIV4 = RIV4 + 1
                IRECL = 4
C                CALL DSIN (ICHAN,IBUFF(1),IV4,IRECL,IER)
C                CALL DSIN (ICHAN,IBUFF(IDXWORD),IV4,IRECL,IER)
                IBUFF(1) = 0
                IBUFF(2) = 0
                CALL DSIN (ICHAN,IBUFF(1),RIV4,IER)
                IF (IER .NE. 0) GO TO 8020
C
C  GET VALUE FOR CELL
550          CONTINUE
                IVALUE(1) = IBUFF(1)
                IVALUE(2) = IBUFF(2)
C             RVALUE = FLOAT(IBUFF(1))
C             IF (NWORDS .LT. 2) GO TO 560
C                IVALUE(1) = IBUFF(IDXWORD)
C                IVALUE(2) = IBUFF(IDXWORD+1)
C
C  FIND BAR VALUE FALLS IN, AND BUMP FREQUENCY COUNT
560          CALL HSTBARVL(RVALUE,BARBRK,BARFRQ,NSUBS,MAPTYPE,CHANGE)
             IDXWORD = IDXWORD + NWORDS
C
600       CONTINUE
C ENDDO COLUMNS
C
700    CONTINUE
C ENDDO ROWS
C     Close cell file
       CALL DSCLS(ICHAN,IER)
C
C     Draw Histogram
800    CALL HSTPLT (BARFRQ,PLTWORK,NSUBS,PLOTPRM(11),IDEVICE)
C      DISPLAY HISTOGRAM LEGEND
850    CALL HSTLEG(IDD,ICHAN,IDEVICE,NSUBS,MAPTYPE,NATRBYT,
     +             BARBRK,BARFRQ,NUMB)
C
       GO TO 9999
C
C******************************************************************
C   ERROR RETURNS
C******************************************************************
C
8010   WRITE (NPRNT,9010) (MAP(I),I=1,10), IER
9010   FORMAT(' *HSTCNT* : ERROR FROM OPEN OF MAP',10A2,'ERROR ',I5)
       ier=1
       GO TO 9999
C
8020   WRITE (NPRNT,9020) IER
9020   FORMAT(' *HSTCNT* : ERROR FROM DSIN  ERROR',I5)
       ier=1
C
9999   RETURN
       END
