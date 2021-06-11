       SUBROUTINE HSTDSC (IDD,ier)
C********************************************************************
C
C       ROUTINE SUBORDINATE TO THE MOSS STATISTICS COMMAND, HANDELS
C               HISTOGRAM LOGIC FOR DESCRETE CELL MAPS
C
C        IDD = STRUCTURE TABLE I.D. OF ACTIVE MAP OF INTEREST
C ********************************************************************
C
       COMMON /IO/ NPRNT, IOIN
C
       INTEGER IBUFF(512)
       INTEGER PLOTPRM(100),NUMB(200)
       INTEGER MAP(25),TITLE(30),IHOLD(4)
C
       REAL RVALUE
       REAL RHOLD(2)
       REAL BARBRK(100,2)
       REAL BARFRQ(100)
       REAL PLTWORK(100)
       REAL CELL,TCELLS
C
       LOGICAL OK,CHANGE
C
C       EQUIVALENCE (IBUFF(88),RHOLD(1))
       EQUIVALENCE (IHOLD(1),RHOLD(1))
C
       ICHAN = 15
       ier=0
       IDEVICE = 1
       IBLNK = 8224
C
C  OPEN .HD FILE
       call gtnam(IDD,5,IBLNK,MAP,IER)
       CALL DSOPN(ICHAN,MAP,1,1,1,256,256,IER)
       IF( IER .NE. 0) GO TO  8010
       CALL DSIN(ICHAN,IBUFF,1.0,IER)
       IF( IER .NE. 0) GO TO 8020
C
C  PARSE OUT INFO
       MAPTYPE = IBUFF(62)
       NROWS = IBUFF(60)
       NCOLS = IBUFF(61)
       NSUBS = IBUFF(67)
       NATRBYT = IBUFF(191)/2
C       NBITS = BYTE(IBUFF(185),1)
C       NBITS = GTBYT(IBUFF(185),1)
       CALL GTB( NBITS, IBUFF(185), 1 )
       ROWLEN = NBITS*NCOLS
       INC = NBITS - 1
C
C  SET INITIAL VALUES
C
C  min and MAX VALUEs for descrete MAPs are range of subjects.
       RHOLD(1) = 1.
       RHOLD(2) = FLOAT(NSUBS)
C
C  INITIATE AND FILL FREQUENCY ARRAY
       DO 100 I=1,100
          BARFRQ(I) = 0.0
100    CONTINUE
C
       PLOTPRM(1) = IBUFF(67)
       PLOTPRM(2) = IBUFF(62)
C  CELL MIN & MAX VALUES (3-6)
       PLOTPRM(3) = IBUFF(88)
       PLOTPRM(4) = IBUFF(89)
       PLOTPRM(5) = IBUFF(90)
       PLOTPRM(6) = IBUFF(91)
C
C PASS MAP DESCRIPTION TO CELPRM
       DO 200 I=1,30
          TITLE(I) = IBUFF(I+19)
200    CONTINUE
       len=30
       CALL LASTC(TITLE,LEN)
       PLOTPRM(11) = LEN
       DO 300 I=1,(LEN+1)/2
          PLOTPRM(I+11) = TITLE(I)
300    CONTINUE
C
C  CLOSE .HD FILE
       CALL DSCLS(ICHAN,IER)
C
C
C  SET UP PARAMETERS FOR CONTINUOUS HISTOGRAM
       OK = .TRUE.
       CALL CELPRM(PLOTPRM,BARBRK,CHANGE,OK)
C
       NSUBS = PLOTPRM(1)
C
C
C  DESCRETE MAP CHECK TO SEE IF SPECIAL HANDLING REQUIRED
       IF ( CHANGE ) GO TO  500
C
C  no CHANGEs so open .CV files and get frequeNCy counts
          call gtnam(IDD,11,IBLNK,MAP,IER)
          CALL DSOPN(ICHAN,MAP,1,1,1,4,128,IER)
          IF (IER .NE. 0 ) GO TO 8010
       IRECL = 8
          DO 400 I=1,NSUBS
           RIV4 = I
             CALL DSIN(ICHAN,IHOLD,RIV4,IER)
             IF (IER .NE. 0) GO TO 8020
                BARFRQ(I) = RHOLD(2)
400       CONTINUE
          CALL DSCLS(ICHAN,IER)
          GO TO 800
C
C  Special CHANGEs requested for descrete MAP, so
C  Open .CD file
500    call gtnam(IDD,7,IBLNK,MAP,IER)
       CALL DSOPN(ICHAN,MAP,1,1,2,NBITS,128,IER)
       IF (IER .NE. 0) GO TO 8010
       IRECL = 1
C
C  CLEAR OUT ARRAYS
       DO 600 I=1,512
          IBUFF(I) = 0
600    CONTINUE
C
C
C  PROCESS EACH CELL
       TCELLS = FLOAT(NROWS)*FLOAT(NCOLS)
       DO 700 CELL = 1.,TCELLS
C
          CALL DSIN (ICHAN,IVALUE,cell,IER)
          IF (IER .NE. 0) GO TO 8020
C
C  GET VALUE FOR CELL
          RVALUE = FLOAT(IVALUE)
C
C  FIND BAR VALUE FALLS IN, AND BUMP FREQUENCY COUNT
          CALL HSTBARVL(RVALUE,BARBRK,BARFRQ,NSUBS,MAPTYPE,CHANGE)
C
C
700    CONTINUE
C ENDDO CELLS
C
C
C     Close cell file
       CALL DSCLS(ICHAN,IER)
C
C     Draw Histogram
800    CALL HSTPLT (BARFRQ,PLTWORK,NSUBS,PLOTPRM(11),IDEVICE)
C
C     Open Descriptor file if there is one
       IF (NATRBYT .EQ. 0 ) GO TO 850
          call gtnam(IDD,12,IBLNK,MAP,IER)
          CALL DSOPN(ICHAN,MAP,1,1,1,NATRBYT,128,IER)
          IF (IER .NE. 0) GO TO 8010
C
C      DISPLAY HISTOGRAM LEGEND
850   CALL HSTLEG(IDD,ICHAN,IDEVICE,NSUBS,MAPTYPE,NATRBYT,
     +             BARBRK,BARFRQ,NUMB)
C
       GO TO 9999
C
C******************************************************************
C   ERROR RETURNS
C
C******************************************************************
C
8010   WRITE (NPRNT,9010) (MAP(I),I=1,10), IER
9010   FORMAT(' *HSTDSC* : ERROR FROM OPEN OF MAP ',10A2,'ERROR ',I5)
       ier=1
       GO TO 9999
C
8020   WRITE (NPRNT,9020) IER
9020   FORMAT(' *HSTDSC* : ERROR FROM DSIN  ERROR',I5)
       ier=1
C
9999   RETURN
       END
