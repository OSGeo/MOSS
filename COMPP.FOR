C**********************************************************************
C 
C     NAME:  SUBROUTINE COMPP
C
C     FUNCTION:  PROCESSOR FOR COMPUTE COMMAND WHICH CREATES A REAL ATTRIBUTE
C                BY COMBINING TWO OR MORE INTEGER OR REAL ATTRIBUTES AND
C                NUMERIC VALUES USING MATHEMATICAL OPERATIONS AND MATHEMATICAL
C                FUNCTIONS FOR COMPUTE COMMAND.
C
C     PARAMETERS:  NAME     USE     TYPE     DIMENSION     RANGE
C                  -------------------------------------------------
C                  IDD       I      I*2                      INTEGER
C                  ACTIVE MAP ID TO PERFORM COMPUTATIONS ON
C                  -------------------------------------------------
C                  ICHOUT    I      I*2                      INTEGER
C                  CHANNEL FOR THE OUTPUT FILE
C                  -------------------------------------------------
C                  ICHMAP    I      I*2                      INTEGER
C                  CHANNEL THE MAP IS OPENED ON
C                  -------------------------------------------------
C                  ICHATT    I      I*2                      INTEGER
C                  CHANNEL THE ATTRIBUTE FILE IS OPENED ON
C                  -------------------------------------------------
C                  NEWREC    I      I*2          128         CHAR/INTEGER
C                  THE ATTRIBUTE HEADER RECORD FOR THE NEW ATTRIBUTE
C                  -------------------------------------------------
C                  JATTR     I      I*2          8,200       CHAR/INTEGER
C                  THE ATTRIBUTE NAME(5), AND TYPE,LENGTH, & FIRST REC
C                  -------------------------------------------------
C                  NFEATS    I      I*2                      INTEGER
C                  NUMBER OF FEATURES IN THE MAP
C                  -------------------------------------------------
C                  NUMATT    I      I*2                      INTEGER
C                  NUMBER OF ATTRIBUTES IN THE ATTRIBUTE FILE
C                  -------------------------------------------------
C                  ISTAK     I      I*2          2,NSYMB     INTEGER
C                  MATH EXPRESSION IN RPN
C                  -------------------------------------------------
C                  NSYMB     I      I*2                      INTEGER
C                  NUMBER OF SYMBOLS IN ISTAK
C                  -------------------------------------------------
C                  IMAP      I      I*2          64          INTEGER
C                  ARRAY POINTED AT IN ISTAK CONTAINING FEATURE ATTRIBUTES
C                  (-1,-2,3) AND MULTIPLE ATTRIBUTE (+ #) POINTERS
C                  -------------------------------------------------
C                  RMAP      I      R*4          64          REAL
C                  ARRAY POINTED AT IN ISTAK CONTAINING NUMERIC CONSTANTS
C                  -------------------------------------------------
C                  IER       O      I*2                      INTEGER
C                  ERROR INDICATOR
C                  -------------------------------------------------
C***********************************************************************
C
       SUBROUTINE COMPP (IDD,ICHOUT,ICHMAP,ICHATT,NEWREC,JATTR,
     1 NFEATS,NUMATT,ISTAK,NSYMB,IMAP,RMAP,IER)
       COMMON /IO/     NPRNT,IOIN
       COMMON /MAXS/   MX(16),RMAX
C
       INTEGER NEWREC(128),MBUFF(128),JATTR(13,200),NSYMB,IER,
     1         NTEMP,ISTAK(2,128),JSTAK(2,128),IMAP(64),ONE,
     2         TWO,ICHOUT,MAYBE,NEXT,NAME(15),kbuff(128),
     3         INDX,IUSED(8),ICHAN(8),ITOTL(2),IVALU(2),NFUNC,FUNC
      REAL MATHFN,RMAP(64),RTOTL,RBUFF(64),ARLEN,PERIM,OLD1,OLD2,WGHT(2)
       LOGICAL FIRST,AVAIL(8),OPERATR
       EQUIVALENCE (ITOTL(1),RTOTL),(IVALU(1),VAL)
      EQUIVALENCE (MBUFF(1),RBUFF(1)),(MBUFF(7),ARLEN),(MBUFF(9),PERIM)
C
       DATA WGHT  / 1.0,1.0 /
C
       NTEMP = 8
       ISTRT = 1
       MXMAP = 8
       NOPEN = 0
       OLD1 = 0.0
       OLD2 = 0.0
       RTOTL = 0.0
C......INITIALIZE CHANNELS ......
       DO 120 I =41,MXMAP+41
            ICHAN(I-40) = I
  120       CONTINUE
C......INITIALIZE TEMPORARY-FILE FLAGS ......
       DO 160  I=1,NTEMP
C...........CLEAR FLAGS TO INDICATE AVAILABLE ......
            AVAIL(I) = .TRUE.
  160       CONTINUE
C
C......REPEAT LOOP UNTIL PROCESSED ENTIRE STACK ......
C
  200  CONTINUE
C......RETRIEVE PARTIAL STACK TO PROCESS ......
      CALL MATHSTK (ISTAK,NSYMB,MXMAP,ISTRT,NUSED,IUSED(1),IBEGN,
     + ILAST,IER)
       IF (IER .NE. 0)  GO TO 81080
C
C......FIND NEXT AVAILABLE TEMPORARY FILE FOR RESULTS ......
       DO 300  I=1,NTEMP
            IF (.NOT. AVAIL(I))  GO TO 300
C................CLEAR FLAG TO INDICATE USED .....
                 AVAIL(I) = .FALSE.
C................STORE NUMBER OF TEMPORARY FILES OPEN ......
                 IF (I .GT. NOPEN)  NOPEN = I
                 IRESLT = MX(16) + I
                 II = - (I)
                 CALL GTNAM (0,II,2hZ8,NAME,IER)
C................OPEN TEMPORARY, CREATE IF NON-EXISTENT ......
                 ichout=ichan(i)
                 CALL openf (ichout,NAME,3,256,2,ier)
                 IF (IER .NE. 0)  GO TO 85020
C................OUTPUT A VALUE TO INITIALIZE FILE ......
                 RTOTL = 0.0
                 kbuff(1)=0
                 kbuff(2)=0
                 call wrblk (ichout,nfeats,kbuff,1,IER)
                 IF (IER .NE. 0) GO TO 85060
                 GO TO 400
  300       CONTINUE
C......ERROR, NO AVAILABLE RESULT FILES ......
       GO TO 81060
C
  400  CONTINUE
C......BRANCH IF NO MAPS REQUIRED FOR PARTIAL STACK ......
       IF (NUSED .EQ. 0)  GO TO 750
C...........OPEN NEXT SET OF REQUIRED MAP FILES ......
            DO 700  I=1,NUSED
                 INDX = ISTAK(1,IUSED(I))
C................CHECK IF TEMPORARY OR REGULAR MAP ......
                 IF (INDX .LE. MX(16))  GO TO 700
C.....................MAP IS TEMPORARY ......
C.....................SET OUTPUT CHANNEL TO THIS TEMPORARY FILE ......
                      II = (INDX - MX(16))
C                      CALL GTNAM (0,-(II),2h  ,NAME,IER)
C  already open    CALL openf(ICHAN(II),NAME,3,4,2,ier)
C                      IF (IER .NE. 0)  GO TO 85020
  700                 CONTINUE
C
  750  CONTINUE
C......CHECK FOR SINGLE OR MULTIPLE OPERANDS IN EXPRESSION ......
       OPERATR = .FALSE.
       DO 800  I=IBEGN,ILAST
C...........CHECK FOR OPERATOR INDICATING MULTIPLE OPERANDS ......
            IF ((ISTAK(1,I) .LT. 0) .AND. (ISTAK(1,I) .GT. -12))
     1           OPERATR = .TRUE.
  800       CONTINUE
C......CASE OF SINGLE OPERAND, FIRST ELEMENT MUST BE OPERAND ......
       IF ((.NOT. OPERATR) .AND. (ISTAK(1,IBEGN) .LE. 0))  GO TO 81030
C
C......PROCESS SINGLE OR MULTIPLE MAP EXPRESSION ......
       NFUNC = 0
       FIRST = .TRUE.
C......PROCESS EACH SELECTED ACTIVE FEATURE IN MAP ......
       CALL APGET(IDD,ISTART,ISTOP,NTOT)
       DO 1900  LATT=ISTART,ISTOP
C...........GET THE INDICATED ACTIVE FEATURE .....
            CALL GETFAST(LATT,RBUFF,NREC,MBUFF,I,I,IER)
            IF (IER .NE. 0) GO TO 80620
C................READ THE ACTUAL FEATURE RECORD .....
                 CALL rdblk(ICHMAP,nrec,MBUFF,1,IER)
                 IF (IER .NE. 0) GO TO 85060
                 IFEAT = MBUFF(5)
C...........RESTORE TEMPORARY STACK TO ORIGINAL STACK ......
            DO 1000  I=IBEGN,ILAST
                 JSTAK(1,I) = ISTAK(1,I)
                 JSTAK(2,I) = ISTAK(2,I)
 1000            CONTINUE
C...........PROCESS STACK ......
            DO 1800  NEXT=IBEGN,ILAST
C................CHECK FOR MULTIPLE OPERAND EXPRESSION ......
                 IF (OPERATR)  GO TO 1100
C.....................PROCESS SINGLE OPERAND EXPRESSION ON LAST SYMBOL .....
                      IF (NEXT .NE. ILAST)  GO TO 1100
C..........................SET UP FOR SPECIAL CASE OF SINGLE OPERAND .....
                           MAYBE = ILAST + 1
                           OLD2  = 0.0
                           GO TO 1150
 1100            CONTINUE
C................CHECK FOR OPERAND OR FUNCTION ......
                 IF ((JSTAK(1,NEXT).GE.0) .OR. (JSTAK(1,NEXT).LE.-12))
     1                     GO TO 1800
C.....................OPERATOR - ASSIGN POSITIVE MODE, SET MAYBE POINTER ....
                      MAYBE = NEXT
                      MODE = - JSTAK(1,NEXT)
C.....................RESET STACK TO ZERO ......
                      JSTAK(1,NEXT) = 0
                      JSTAK(2,NEXT) = 0
C................BINARY OPERATION REQUIRES TWO VALUE OPERANDS ......
 1150            CONTINUE
C.....................SEARCH FOR FIRST OPERAND ......
                      MAYBE = MAYBE - 1
                      IF (MAYBE .LT. 1)  GO TO 81040
C.....................COUNT EACH FUNCTION INCURRED ......
                      IF (JSTAK(1,MAYBE) .LE. -12)  NFUNC = NFUNC + 1
C.....................REPEAT UNTIL POP VALUE FROM STACK ......
                      IF (JSTAK(1,MAYBE) .LE. 0)  GO TO 1150
                 ONE = JSTAK(1,MAYBE)
C................IF TEMPORARY FILE, SET AVAILABLE FLAG TO FREE .....
                 IF (((FIRST))  .AND.  (ONE .GT. MX(16))  .AND.
     1                ONE .NE. IRESLT)  AVAIL(ONE-MX(16)) = .TRUE.

C................CHECK FOR MAP OR VALUE ......
                 IF (JSTAK(2,MAYBE) .NE. 1)  GO TO 1250
C.....................CHECK FOR CURRENT RESULT FILE ......
                      IF (ONE .NE. IRESLT)  GO TO 1200
C..........................RETAIN VALUE OF CURRENT RESULT ......
                           OLD1 = RTOTL
                           GO TO 1300
 1200                 CONTINUE
C.....................USE TEMP,MAP ATTRIBUTE, OR ATTRIBUTE FROM ATT FILE ......
                      IF (ONE .LE. MX(16))  GO TO 1220
                      CALL rdblk(ICHAN(ONE-MX(16)),ifeat,kbuff,1,IER)
                      ivalu(1)=kbuff(1)
                      ivalu(2)=kbuff(2)
                      IF (IER .NE. 0)  GO TO 85040
                      OLD1 = VAL
                      GO TO 1300
C
 1220                 OLD1 = ARLEN
                      IF (IMAP(ONE) .LT.-2) OLD1 = PERIM
                      IF (IMAP(ONE) .LT. 0) GO TO 1300
C
                      CALL COMPAT(ICHATT,JATTR,IFEAT,IMAP(ONE),OLD1,IER)
                      IF (IER .NE. 0)  GO TO 80800
                      GO TO 1300
 1250            CONTINUE
C................RETRIEVE REAL VALUE ......
                 OLD1 = RMAP(ONE)
C
 1300            CONTINUE
C................STORE CURRENT STACK INDEX ......
                 INDX = MAYBE
 1350            CONTINUE
C................CHECK FOR REMAINING FUNCTIONS ......
                 IF (NFUNC .EQ. 0)  GO TO 1400
                      INDX = INDX + 1
                      IF (JSTAK(1,INDX) .GT. -12)  GO TO 1350
C..........................PERFORM FUNCTION ON CURRENT VALUE.....
                           FUNC = - JSTAK(1,INDX) - 11
                           OLD1 = MATHFN (OLD1,OLD2,WGHT(1),FUNC)
                           NFUNC = NFUNC - 1
C..........................RESET STACK TO ZERO ......
                           JSTAK(1,INDX) = 0
                           JSTAK(2,INDX) = 0
                           GO TO 1350
 1400            CONTINUE
C................RESET STACK TO ZERO ......
                 JSTAK(1,MAYBE) = 0
                 JSTAK(2,MAYBE) = 0
C................CHECK FOR MULTIPLE OPERAND EXPRESSION ......
                 IF (OPERATR)  GO TO 1450
C.....................PROCESSING COMPLETED FOR SINGLE OPERAND ......
                      RTOTL = OLD1
C.....................BRANCH TO OUTPUT FINAL RESULT ......
                      GO TO 1750
 1450            CONTINUE
C.....................SEARCH FOR SECOND OPERAND ......
                      MAYBE = MAYBE - 1
                      IF (MAYBE .LT. 1)  GO TO 81040
C.....................COUNT EACH FUNCTION INCURRED ......
                      IF (JSTAK(1,MAYBE) .LE. -12)  NFUNC = NFUNC + 1
C.....................REPEAT UNTIL POP VALUE FROM STACK ......
                      IF (JSTAK(1,MAYBE) .LE. 0)  GO TO 1450
                 TWO = JSTAK(1,MAYBE)
C................IF TEMPORARY FILE, SET AVAILABLE FLAG TO FREE .....
                 IF (((FIRST))  .AND. (TWO .GT. MX(16))  .AND.
     1                TWO .NE. IRESLT) AVAIL(TWO-MX(16)) = .TRUE.
C................CHECK FOR MAP OR VALUE ......
                 IF (JSTAK(2,MAYBE) .NE. 1)  GO TO 1550
C.....................CHECK FOR CURRENT RESULT FILE ......
                      IF (TWO .NE. IRESLT)  GO TO 1500
C..........................RETAIN VALUE OF CURRENT RESULT ......
                           OLD2 = RTOTL
                           GO TO 1600
 1500                 CONTINUE
C.....................USE TEMP,MAP ATTRIBUTE, OR ATTRIBUTE FROM ATT FILE ......
                      IF (TWO .LE. MX(16))  GO TO 1520
                      CALL rdblk(ICHAN(TWO-MX(16)),ifeat,kbuff,1,IER)
                      ivalu(1)=kbuff(1)
                      ivalu(2)=kbuff(2)
                      IF (IER .NE. 0)  GO TO 85040
                      OLD2 = VAL
                      GO TO 1600
C
 1520                 OLD2 = ARLEN
                      IF (IMAP(TWO) .LT.-2) OLD2 = PERIM
                      IF (IMAP(TWO) .LT. 0) GO TO 1600
                      CALL COMPAT(ICHATT,JATTR,IFEAT,IMAP(TWO),OLD2,IER)
                      IF (IER .NE. 0)  GO TO 80800
                      GO TO 1600
 1550            CONTINUE
C................RETRIEVE REAL VALUE ......
                 OLD2 = RMAP(TWO)
C
 1600            CONTINUE
C................STORE CURRENT STACK INDEX ......
                 INDX = MAYBE
 1650            CONTINUE
C................CHECK FOR REMAINING FUNCTIONS ......
                 IF (NFUNC .EQ. 0)  GO TO 1700
                      INDX = INDX + 1
                      IF (JSTAK(1,INDX) .GT. -12)  GO TO 1650
C..........................PERFORM FUNCTION ON CURRENT CELL ......
                           FUNC = - JSTAK(1,INDX) - 11
                           OLD2 = MATHFN (OLD2,OLD2,WGHT(1),FUNC)
                           NFUNC = NFUNC - 1
C..........................RESET STACK TO ZERO ......
                           JSTAK(1,INDX) = 0
                           JSTAK(2,INDX) = 0
                           GO TO 1650
 1700            CONTINUE
C................RESET STACK INDEX TO OUTPUT FILE ......
                 JSTAK(1,MAYBE) = IRESLT
                 JSTAK(2,MAYBE) = 1
C................SWITCH ORDER OF VALUES FOR ASSOCIATIVE PROPERTY .....
                 MODE = MODE + 10
                 RTOTL = MATHFN (OLD2,OLD1,WGHT(1),MODE)
 1750            CONTINUE
 1800            CONTINUE
C
C................OUTPUT RESULT TO TEMPORARY FILE ......
                 CALL wrblk(ICHOUT,ifeat,ITOTL,1,IER)
                 IF (IER .NE. 0) GO TO 85060
            IF (FIRST)  FIRST = .FALSE.
 1900       CONTINUE
C
C......CHECK FOR NO TEMPORARIES USED IN PARTIAL STACK ......
       IF (NUSED .EQ. 0)  GO TO 2150
C...........CLOSE TEMP FILES USED IN PARTIAL STACK ......
            DO 2100  I=NUSED,1,-1
                 INDX = ISTAK(1,IUSED(I))
                 IF (INDX .LE. MX(16)) GO TO 2100
C................CLOSE TEMPORARY FILE ......
C                 close(ICHAN(INDX-MX(16)),iostat=IER)
C                 IF (IER .NE. 0)  GO TO 85080
 2100            CONTINUE
C
 2150  CONTINUE
C
C......REPLACE ORIGINAL STACK WITH REVISED PARTIAL STACK ......
       DO 2180  I=IBEGN,ILAST
            ISTAK(1,I) = JSTAK(1,I)
            ISTAK(2,I) = JSTAK(2,I)
 2180       CONTINUE
C
C......CHECK FOR COMPLETED STACK PROCESSING ......
       IF (ILAST .EQ. NSYMB)  GO TO 2200
C...........CLOSE TEMPORARY RESULT FILE ......
C            close(ICHOUT,iostat=IER)
C            IF (IER .NE. 0)  GO TO 85080
C...........REPEAT LOOP TO CONTINUE STACK PROCESSING ......
            GO TO 200
C
C...........WRITE THE RESULTS TO THE ATTRIBUTE FILE .....
 2200       CONTINUE
            NREC = NEWREC(86)
            CALL CLEAR(MBUFF,128)
            J = 0
            DO 2000 I=1,NFEATS
                 CALL rdblk(ICHOUT,I,kbuff,1,IER)
                 itotl(1)=kbuff(1)
                 itotl(2)=kbuff(2)
                 IF (IER .NE. 0)  GO TO 85040
                 J = J + 1
                 IF (J .LE. 64) RBUFF(J) = RTOTL
                 IF (J .LE. 64) GO TO 2000
C................DUMP THE ATTRIBUTE DATA RECORD .....
                     CALL wrblk(ICHATT,nrec,MBUFF,1,IER)
                     IF (IER .NE. 0) GO TO 85060
                     NREC = NREC + 1
                     J = 1
                     CALL CLEAR(MBUFF,128)
                     RBUFF(J) = RTOTL
 2000            CONTINUE
C...........MAKE SURE TO DUMP THE LAST RECORD .....
            IF (J .NE. 0) CALL wrblk(ICHATT,nrec,MBUFF,1,IER)
            IF (IER .NE. 0) GO TO 85060
C......UPDATE THE ATTRIBUTE HEADER RECORD WITH THE NEW ATTRIBUTE COUNT ...
       CALL rdblk(ICHATT,1,MBUFF,1,IER)
       IF (IER .NE. 0) GO TO 85040
       MBUFF(1) = MBUFF(1) + 1
       CALL wrblk(ICHATT,1,MBUFF,1,IER)
       IF (IER .NE. 0) GO TO 85060
C
C......CLOSE THE LAST RESULT FILE ......
C       close(ICHOUT,iostat=IER)
C
 2300  CONTINUE
C......RETURN ......
       GO TO 90000
C
C......ERROR RETURNS ......
C
80600  CONTINUE
       IER = -5640
       WRITE (NPRNT,98060)  IER
98060  FORMAT (' *COMPP*  NO MAP NAMES IN STACK  ERROR',I5)
       GO TO 90000
C
80620  CONTINUE
       WRITE (NPRNT,98062)  IER
98062  FORMAT (' *COMPP*  ERROR FROM GETFAST  ERROR',I5)
       GO TO 90000
C
80700  CONTINUE
       WRITE (NPRNT,98070)  IER
98070  FORMAT (' *COMPP*  ERROR FROM DFILW  ERROR',I5)
       GO TO 90000
C
80800  CONTINUE
       WRITE (NPRNT,98080)  IER
98080  FORMAT (' *COMPP*  ERROR FROM COMPAT READING ATTRIBUTES  ERROR',
     +   I5)
       GO TO 90000
C
81030  CONTINUE
       IER = -5642
       WRITE (NPRNT,98103)  IER
98103  FORMAT (' *COMPP*  FUNCTION REQUIRES ONE MAP OPERAND  ERROR',I5)
       GO TO 90000
C
81040  CONTINUE
       IER = -5644
       WRITE (NPRNT,98104)  IER
98104  FORMAT (' *COMPP*  OPERATION REQUIRES TWO MAP OPERANDS-ERROR',I5)
       GO TO 90000
C
81060  CONTINUE
       IER = -5644
       WRITE (NPRNT,98106)  NTEMP,IER
98106  FORMAT (' *COMPP* EXPRESSION REQUIRES MORE STORAGE BUFFERS THAN',
     1         ' MAXIMUM OF',I2,'  ERROR',I5)
       GO TO 90000
C
81080  CONTINUE
       WRITE (NPRNT,98108)  IER
98108  FORMAT (' *COMPP*  ERROR FROM MATHSTK  ERROR',I5)
       GO TO 90000
C
85020  CONTINUE
       WRITE (NPRNT,98502)  (NAME(I),I=1,MX(5)),IER
98502  FORMAT(' *COMPP*  ERROR FROM DSOPN OF ',10A2,' ERROR',I5)
       GO TO 90000
C
85040  CONTINUE
       WRITE (NPRNT,98504)  IER
98504  FORMAT (' *COMPP*  ERROR FROM DSIN  ERROR',I5)
       GO TO 90000
C
85060  CONTINUE
       WRITE (NPRNT,98506)  IER
98506  FORMAT (' *COMPP*  ERROR FROM DSOUT  ERROR',I5)
       GO TO 90000
C
85080  CONTINUE
       WRITE (NPRNT,98508)  IER
98508  FORMAT (' *COMPP*  ERROR FROM DSCLS  ERROR',I5)
       GO TO 90000
C
C......NORMAL RETURN ......
C
90000  return
       END
