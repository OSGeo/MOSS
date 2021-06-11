C***********************************************************************
C
C     NAME:  SUBROUTINE DESC
C
C     FUNCTION:  DISPLAYS THE HEADER, PROJECTION, SUBJECTS, ATTRIBUTES
C                OF MAPS.
C
C     PARAMETERS:  NAME     USE     TYPE     DIMENSION     RANGE
C                  -------------------------------------------------
C                  JCOM      I      I*2         10         CHAR
C                  MAP NAME IF IFLAG IS ONE
C                  -------------------------------------------------
C                  IFLAG     I      I*2                    0,1
C                  0-INDICATES NORMAL  1-INDICATES FROM LIST & NAME IN JCOM
C                  -------------------------------------------------
C                  LINES     I/O    I*2                    +INTEGER
C                  NUMBER OF LINES OF OUTPUT
C                  -------------------------------------------------
C                  IER       O      I*2                    INTEGER
C                  ERROR FLAG
C                  -------------------------------------------------
C
C     FILES:  MAP FILE
C
C     ROUTINES REQUIRED:  OPENF  RDBLK  INITIAL  LISTAT  GTNAM  MCOMP  
C                         DESHDR  DESCPRJ  DESCSB  DESVSB  CALERR  MCHEK1
C
C     CALLING ROUTINE:  CTERP  LIST
C
C     SYSTEM DEPENDENCE:  2-BYTE INTEGER
C
C     LANGUAGE DEPENDENCE:  FORTRAN
C
C***********************************************************************
C
        SUBROUTINE DESC(LINES,IER)
$Include:'DNAMES.inc'
$Include:'IO.inc'
$Include:'STRNG.inc'
$Include:'WORK.inc'
        COMMON /UNIT/  CHANEL,ATRBTE,POLYGON,ADRESS
C
        INTEGER IHEAD(256),WRK(25)
        INTEGER CHANEL,ATRBTE,POLYGON,ADRESS
        LOGICAL MCOMP
C
        JCHAN = 32
        CHANEL= 31
C
  100   CONTINUE
        CALL CLEAR(WRK,25)
        CALL CLEAR(JCOM,10)
        CALL DECIP(ICARD,JCOM,ICARDP)
        IF (JCOM(1) .NE. 0) GO TO 2000
C
C  PRMPT USER FOR NAME
C
    2   WRITE(NPRNT,90100)
90100   FORMAT(/,' ENTER MAPNAME TO LIST'\)
        CALL READIN(ICARD,LENICD,ICARDP,istop)
        LINES = LINES + 3
        if(icard(1).eq.0)go to 90000
        go to 100
C
C  CHECK FOR VALID MAP IN LIST <MAPNAME>.
C
 2000   CONTINUE
        CALL MCHEK(JCOM,IFIL,NENTS,ITYPE,IER)
C
C  EXIT IF NOT FOUND
C
        IF (IFIL .EQ. 0) THEN
          WRITE(NPRNT,92000)
92000     FORMAT(' THIS MAP DOES NOT EXIST')
          LINES = LINES + 2
          GO TO 90000
        ENDIF
C
C  THIS MAP EXISTS BUT IS IT ARCHIVED ?
C
 2100   CONTINUE
        IF(NENTS.LT.0) GO TO 80100
C
C  OPEN MAP FILE AND READ HEADER
C
        IF (ITYPE .GE. 6  .AND.  ITYPE .LE. 9) then
            CALL GTNAM(-1,5,JCOM,MAP,IER)
        endif
C
C If file in master dir. put dir. prefix on it
C
         IF ( IFIL .EQ. 1 ) THEN
             jcom(1)=(-jcom(1))
            CALL ADDDIR(JCOM,WRK)
         ELSE
            INDEX = 1
            DO 200 I=1,10
               IF( JCOM(I) .NE. 0 ) THEN
                  WRK(INDEX)=JCOM(I)
                  INDEX = INDEX + 1
               ENDIF
200         CONTINUE
         ENDIF
C
        IF (ITYPE .LT. 6  .OR.   ITYPE .GT. 9)
     +   CALL PACKC(WRK,MAP,25)
        CALL OPENF(JCHAN,MAP,1,256,2,IER)
        IF(IER.NE.0) GO TO 85020
        CALL RDBLK(JCHAN,1,IHEAD(1  ),1,IER)
        IF (IER .NE. 0) GO TO 85040
        CALL RDBLK(JCHAN,2,IHEAD(129),1,IER)
        IF (IER .NE. 0) GO TO 85040
C        CALL GTNAM(-1,0,JCOM,MAP,IER)
C
C  DID THEY SPECIFY THE NEXT STUFF
C
        CALL DECIP(ICARD,ICOM,ICARDP)
        IF(ICOM(1).NE.0) GO TO 2200
C
C  PROMPT FOR ACTION
C
    1   WRITE(NPRNT,90220)
90220 FORMAT(/,' ENTER ACTION: HEADER, PROJECTION, SUBJECTS,',
     +         ' ATTRIBUTES '\)
        lines=0
        CALL READIN(ICARD,LENICD,ICARDP,istop)
   11   CALL DECIP(ICARD,ICOM,ICARDP)
        if(icom(1).eq.0)go to 90000
        LINES=LINES+2
C
C  GOT ACTION.  NOW BRANCH
C
 2200   CONTINUE
        NENTS = 0
        IF(MCOMP(ICOM,'H',1,IER))  NENTS = 1
        IF(MCOMP(ICOM,'P',1,IER))  NENTS = 2
        IF(MCOMP(ICOM,'S',1,IER))  NENTS = 3
        IF(MCOMP(ICOM,'AT',2,IER)) NENTS = 4
C        IF(MCOMP(ICOM,'AS',2,IER)) NENTS = 5
C
        GO TO (3100,3200,3300,3400,3500), NENTS
C
C  INPUT NOT HEADER PROJECTION, SUBJECTS, NOR ATTRIBUTES
C
        WRITE(NPRNT,90240)
90240   FORMAT(' YOUR CHOICE FOR ACTION MUST BE:',/,
     +         ' HEADER, PROJECTION, SUBJECTS, OR ATTRIBUTES')
        LINES = LINES + 2
        GO TO 1
C
C  DISPLAY HEADER FOR THIS MAP
C
 3100   CONTINUE
        CALL DESHDR(IHEAD,MAP,LINES,IER)
        GO TO 11
C
C  DISPLAY PROJECTION FOR THIS MAP
C
 3200   CONTINUE
        CALL DESPRJ(IHEAD,MAP,LINES,IER)
        GO TO 11
C
C  DISPLAY SUBJECTS FOR THIS MAP
C
 3300   CONTINUE
C        IF(ITYPE .GE. 6  .AND.  ITYPE .LE. 9) GO TO 3340
        CALL DESVSB(IHEAD,MAP,JCHAN,LINES,IER)
        GO TO 11
C
C  DISPLAY SUBJECTS FOR DISCRETE CELL MAP <not applicable in micro>
C
C 3340   CONTINUE
C        CLOSE(JCHAN,iostat=IER)
C        IF (ITYPE .NE. 7) WRITE(NPRNT,90334)
C90334   FORMAT(' ONLY DISCRETE CELL MAPS HAVE SUBJECTS',/,
C     +         ' DICHOTOMOUS, CONTINUOUS, AND MULTIVALUE CELL MAPS DO NOT')
C        IF (ITYPE .NE. 7) LINES = LINES + 2
C        IF (ITYPE .NE. 7) GO TO 90000
C        CALL DESCSB(IHEAD,MAP,JCHAN,CHANEL,LINES,IER)
C        GO TO 90000
C
C  ATTRIBUTES  FIRST CHECK TO SEE WHETHER MAP HAS ATTRIBUTES
C
 3400   CONTINUE
C
C  IT HAS AN ATTRIBUTE FILE.  CREATE EXTENSION AND OPEN
C
        CALL GTNAM(-1,4,JCOM,MAP,IER)
        IF(IHEAD(76).NE.1) GO TO 80120
        CALL OPENF(CHANEL,MAP,1,256,2,IER)
        IF(IER.NE.0) GO TO 85020
C
C  INITIALIZE ATTRIBUTE FILE INFO
C
        CALL INITIAL
C
C  AND CALL ATTRIBUTE LISTING ROUTINE
C
        CALL LISTAT
        LINES=0
        GO TO 11
C
C  ASSIGNMENT LIST
C
 3500   CONTINUE
        IFIL = MOD(ITYPE,10)
        IF (IFIL .NE. 1  .AND.  IFIL .NE. 2  .AND.  IFIL .NE. 3)
     +  WRITE(NPRNT,93500) ITYPE
93500   FORMAT(' ONLY VECTOR MAPS HAVE ASSIGNMENTS NOT TYPE',I5)
        IF (IFIL .EQ. 1  .OR.  IFIL .EQ. 2  .OR.  IFIL .EQ. 3)
     +     CALL LSTASN(LINES,IFIL,JCHAN,IER)
        GO TO 11
C
C  ERROR RETURNS
C
80100  write(*,98500)(map(l),l=1,12)
98500  format(1x,'This map has been archived: ',12a2)
       GO TO 2
C
80120  write(*,98501)(map(l),l=1,12)
98501  format(1x,'This map does not have a multiple attribute file: ',
     + 12a2)
       GO TO 11
C
85020  CONTINUE
       WRITE(NPRNT,98502)  (MAP(IFIL),IFIL=1,12),IER
98502  FORMAT(' *DESC*   ERROR OPENING FILE ',12A2,'  ERROR',I5)
       LINES = LINES + 2
       GO TO 2
C
85040  CONTINUE
       WRITE(NPRNT,98504)  IER
98504  FORMAT(' *DESC*   ERROR READING MAP HEADER  ERROR',I5)
       LINES = LINES + 2
C
C  JUMP OUT
C
90000  CONTINUE
       close(jchan,iostat=ier)
       close(chanel,iostat=ier)
       RETURN
       END
