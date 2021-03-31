C************************************************************************
C
C  Name:    L B P A R M
C
C  Purpose: THIS WILL READ ALL OF THE EMBEDDED OR PROMPTED
C           INPUT OPTIONS NECESSARY FOR THE CONTOUR LABELING PROCESS.
C
C  Commons/Variables    In/Out        Attributes   Function/Size
C
C  -- Dummy Arguments --
C     NOMASK            Out           Logical - Screen output flag
C     LOSER             Out           Integer - Level of resolving conflicts
C     CHRSZ             Out           Real    - Character size
C     IMINI             Out           Integer - Minimum value subject
C     IINTER            Out           Integer - Interval to label
C     MTXT              Out           Integer Array(25) - Text map name
C     LNMASK            Out           Integer Array(25) - Masked line map name
C     IDD               Out           Integer - Active ID data pointer
C     IER               Out           Integer - Returned error code
C
C  -- IO --
C     NPRNT                           Integer
C     IOIN                            Integer
C
C  -- DEBUG --
C     IDEB                            Integer
C
C  -- TABLE --
C     ITABLE                          Integer Array         40
C
C  -- TYPE --
C     IDATYP                          Integer Array         40
C     NACTS                           Integer
C
C  -- STRNG --
C     ICARD                           Integer Array         80
C     ICARDP                          Integer
C
C  -- WHMAP --
C     ISTRCT                          Integer Array        300
C     NSTRCT                          Integer
C
C  -- WORK --
C     IBUFF                           Integer Array        128
C     NAME                            Integer Array         25
C     ICOM                            Integer Array         10
C     JCOM                            Integer Array         10
C
C  -- Stack Variables --
C     IOK                             Logical
C     I                               Integer
C     ICC                             Integer
C     MACTS                           Integer
C     MTYPE                           Integer
C     LINES                           Integer
C     NPOS                            Integer
C     IMASK                           Integer
C     IMS                             Integer Array          2
C
C  --   external Subprograms --
C     DAGET          Qreadin         CLEAR          Qdecip          Qwhtcal
C     INUM           CFILW          DFILW
C
C  Files:
C       Unit    File name or description
C       ------  -------------------------------------------------------
C       10      "@CONSOLE" for output
C       11      "@CONSOLE" for input
C
C  Calling:          AUTOLB
C
C  Notes:            None
C******************************************************************************
C
       SUBROUTINE LBPARM(NOMASK,LOSER,CHRSZ,IMINI,IINTER,MTXT,LNMASK,
     +                   IDD,IER)
       COMMON /IO/    NPRNT,IOIN
       COMMON /TABLE/ ITABLE(40)
       COMMON /TYPE/  IDATYP(40),NACTS
       COMMON /STRNG/ ICARD(80),ICARDP,LENICD
       COMMON /WHMAP/ ISTRCT(300),NSTRCT
       COMMON /WORK/  IBUFF(128),NAME(25),ICOM(10),JCOM(10)
C
       LOGICAL   MCOMP, NOMASK, IOK, BAD
       DIMENSION IMS(2), MTXT(25), LNMASK(25)
       CHARACTER NO
C
      IER     = 0
      NO = 'N'
C
C.....CHECK TO SEE IF ANY ACTIVE MAPS
      IF (NACTS .LT. 1) GOTO 3000
C
C.....CALL DAGET TO GET ID
      CALL DAGET(ICARD,ICARDP,ITABLE,40,MACTS)
      IF (MACTS .GT. 0) GO TO 20
C
        WRITE(NPRNT,9)
    9   FORMAT(' Enter active ID to AUTOLABEL'\)
        call readin(ICARD,80,ICARDP,LENICD)
        CALL DAGET(ICARD,ICARDP,ITABLE,40,MACTS)
C
C.......DID THEY ACTUALLY GIVE ME A NUMBER?
        IF(MACTS .LE. 0) GOTO 9000
C
C.....OK, GOT A VALID ACTIVE MAP... NOW SET AND CHECK THE MAP TYPE
   20 IDD   = ITABLE(1)
      MTYPE = IDATYP((IDD/7)+1)
      IF (MTYPE .NE. 2) GO TO 3010
C
C.....NOW WE NEED THE TEXT MAP NAME ... IS IT ON THE COMMAND LINE?
      CALL CLEAR(ICOM,10)
C      call decip(ICARD,ICOM,ICARDP)
       call clear(icard,80)
      IF (ICOM(1) .EQ. 0) GO TO 30
        GO TO 50
C.....PROMPT MODE ONLY
   30 WRITE(NPRNT,31)
   31 FORMAT(' Do you want output to the screen ? (Y/N) [CR=Y] '\)
      call readin (Iget,1,ICARDP,LENICD)
      IF(MCOMP(Iget,NO,1,IER)) GO TO 32
      NOMASK = .TRUE.
      BAD=.FALSE.
      IVAL=1
      CALL CLEAR(ICOM,10)
      GO TO 182
32    call whtcal(ICOM,IIER)
C
C.....WE HAVE A NAME, NOW PACK IT AND SEE IF IT IS TOO LONG (MAX 7 CH)
   50 CALL CLEAR ( MTXT, 25 )
      CALL CLEAR ( LNMASK, 25 )
      CALL PACKC(ICOM,lnmask,10)
C      icc=25
C      CALL LASTC(MTXT,ICC)
C      IF (ICC .LE. 8) GO TO 75
C.......NAME TOO LONG FOR ".MS" EXTENSION
C        WRITE(NPRNT,53) (ICOM(I),I=1,10)
C   53   FORMAT(' NAME ',10A2,' TOO LONG.  MUST BE 7 OR LESS',
C     +         ' CHARACTERS.')
C        CALL CLEAR(ICOM,10)
C        GO TO 30
C
C.....SETUP LINE MASK FILENAME JUST IN CASE
C   75 DO 80 I = 1,10
C        MTXT(I)=ICOM(I)
C   80   LNMASK(I) = MTXT(I)
C.....PREPARE EXTENTION FOR LINE MASK NAME
      CALL GTNAM(-1,18,ICOM,mtxt,IER)
C.....IF SCREEN OUTPUT DESIRED SET FLAG - MAY BE OVERRIDDEN BY MASK=Y
      ISC=2HSC
      IRE=2HRE
      IF ( MTXT(1).EQ.ISC .AND. MTXT(2).EQ.IRE ) NOMASK = .TRUE.
C
C.....NOW TO GET REST OF THE OPTIONS FROM THE COMMAND LINE
      BAD = .FALSE.
      IVAL = 0
      ICOM(1)=0
      GO TO 175
C
  150 call readin ( ICARD, 80, ICARDP, LENICD)
  175 IF ( .NOT.BAD ) IVAL = IVAL + 1
      CALL CLEAR (ICOM,10)
C      call decip(ICARD,ICOM,ICARDP+1)
      call decip(ICARD,ICOM,ICARDP)
C
C.....SOMETHING ON COMMAND LINE OR SOMETHING AFTER PROMPT
      IF ( ICOM(1).NE.0 ) THEN
        GO TO ( 200, 300, 400, 500, 1000 ), IVAL
        ENDIF
C
C.....NOTHING ON COMMAND LINE - PROMPT
  182 IF ( .NOT.BAD .AND. ICOM(1).EQ.0 ) THEN
        GO TO ( 250, 350, 450, 550, 1000 ), IVAL
        ENDIF
C
C.....NO RESPONSE TO PROMPT - SET DEFAULT
      IF ( BAD .AND. ICOM(1).EQ.0 ) THEN
        GO TO ( 275, 375, 475, 575, 1000 ), IVAL
        ENDIF
C
C.....HIGHLIGHT INTERVAL SECTION
  200 BAD = .FALSE.
      M = 0
      CALL INUM ( ICOM, IINTER, M )
      IF ( IINTER.GT.0 ) GO TO 175
        BAD = .TRUE.
        WRITE(NPRNT,213) IINTER
  213   FORMAT(' INVALID HIGHLIGHT INTERVAL ',I6,', RE-ENTER [5] '\)
        GO TO 150
C.....PROMPT FOR INTERVAL
  250 WRITE(NPRNT,253)
  253 FORMAT(' Labeling interval desired [5] '\)
      BAD = .TRUE.
      GO TO 150
C.....SET IINTER TO DEFAULT VALUE
  275 IINTER = 5
      BAD = .FALSE.
      GO TO 175
C
C.....LABELING MINIMUM SECTION
  300 BAD = .FALSE.
      M = 0
      CALL INUM ( ICOM, IMINI, M )
      IF ( M .GT. 1 ) GO TO 175
      BAD = .TRUE.
      WRITE(*,3333) IMINI
 3333 FORMAT(' LABELING MINIMUM OUT OF RANGE ', I6,' RE-ENTER[0]'\)
      GOTO 150
C.....PROMPT FOR MINIMUM
  350 WRITE(NPRNT,353)
  353 FORMAT(' Labeling minimum value [0]: '\)
      BAD = .TRUE.
      GO TO 150
C.....SET DEFAULT MINIMUM
  375 IMINI = 0
      BAD = .FALSE.
      GO TO 175
C
C.....CONFLICT RESOLVING SECTION
  400 BAD = .FALSE.
      M = 0
      CALL INUM ( ICOM, LOSER, M )
      IF ( LOSER.GE.0 .AND. LOSER.LE.3 .AND. M.GT.1 ) GO TO 175
        BAD = .TRUE.
        WRITE(NPRNT,403) LOSER
  403   FORMAT(' INVALID RESOLVE LEVEL ',I6,', RE-ENTER (0-3) [0] '\)
        GO TO 150
C.....PROMPT FOR CONFLICT RESOLVING LEVEL
  450 WRITE(NPRNT,453)
  453 FORMAT(' Conflict resolve level (0-3) [0 (none)] '\)
      BAD = .TRUE.
      GO TO 150
C.....SET DEFAULT RESOLVE LEVEL
  475 LOSER = 0
      BAD = .FALSE.
      GO TO 175
C
C.....CHARACTER SIZE SECTION
  500 BAD = .FALSE.
      M = 0
      CALL FNUM ( ICOM, 10, CHRSZ, M )
      IF ( CHRSZ.GE..01 .AND. CHRSZ.LE..5 ) GO TO 175
        WRITE(NPRNT,503) CHRSZ
  503   FORMAT(' CHARACTER SIZE ',F6.3,', OUT OF RANGE,RE-ENTER [.13]'\)
        BAD = .TRUE.
        GO TO 150
C.....PROMPT FOR CHARACTER SIZE
  550 WRITE(NPRNT,553)
  553 FORMAT(' Character size (.01-.5) [.13] '\)
      BAD = .TRUE.
      GO TO 150
C.....SET DEFAULT CHARACTER SIZE
  575 CHRSZ = .13
      BAD = .FALSE.
      GO TO 175
C
C.....CHECK ON EXISTENCE OF FILENAMES AND USERS DIRECTIVES
C
1000   GO TO 9000
c 1000 IF ( NOMASK ) GO TO 9000
      CALL openf(18,mtxt,0,0,0,ier)
      IF ( IER.ne.0 ) GO TO 1050
        WRITE(NPRNT,1047) (MTXT(I),I=1,10)
 1047   FORMAT(' Text file ',10A2,' exists, overwrite it (Y/N) [Y] '\)
        call readin ( ICARD, 80, ICARDP, LENICD )
        IER = 0
        IF ( MCOMP(ICARD(1),'N',1,IER) ) IER = 5
        IF ( IER.NE.0 ) GO TO 9000
C.......TEXT FILE MUST EXIST
        CALL openf(18,MTXT,0,0,0,IER )
        close(18)
C
C.....NOW TRY TO CREATE IT IN THE SYSTEM
 1050 CALL openf(18,LNMASK,0,0,0,IER)
      IF (IER .ne. 0) GO TO 1100
        WRITE(NPRNT,1053) (LNMASK(I),I=1,15)
 1053   FORMAT(' Mask file ',15A2,' exists, overwrite it (Y/N) [Y] '\)
        call readin(ICARD,80,ICARDP,LENICD)
        IER = 0
        IF (MCOMP(ICARD(1),'N',1,IER)) IER = 6
        IF (IER.NE.0 ) GO TO 9000
C
C.......INSURE LNMASK DOES NOT EXIST - SAVWRK CREATES IT
 1100  ier=0
       go to 9000
C
C.....ERROR MESSAGES
C
 3000 WRITE(NPRNT,3001)
 3001 FORMAT(' YOU MUST SELECT A MAP BEFORE ENTERING AUTOLABEL')
      IER = 3
      GO TO 9000
C
 3010 WRITE(NPRNT,3011)
 3011 FORMAT(' THE MAP TO LABEL MUST BE LINE TYPE (TYPE 2)  *LBPARM*')
      IER = 4
C
 9000  continue
cIF ( IDEB.GT.0 ) WRITE(NPRNT,9003) IINTER, IMINI, LOSER, CHRSZ
c 9003 FORMAT(' IINTER, IMINI, LOSER & CHRSZ = ',3I6,F6.3)
      RETURN
      END
