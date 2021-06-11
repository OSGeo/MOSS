C ****************************************************************
C
C             P R O G R A M     L I S T
C
C  FUNCTION- LIST OUT MAPS IN MAP FILES OR SUJECTS OR
C
C  INPUT PARAMETERS:
C
C        LINES - CURSOR INFO
C
C  REQUIRED SUBROUTINES: LSTDIR  OPENF  MCOMP  MCOMP1 DESC
C
C **************************************************************
      PROGRAM list
C
$Include:'DNAMES.inc'
$Include:'IO.inc'
$Include:'STRNG.inc'
$Include:'WORK.inc'
      COMMON /fls   / iunit , ifile(5)
      COMMON /dbmap / dbnmap(2)
      INTEGER dbnmap
      LOGICAL mcomp1 , mcomp
C
C  INITIALIZE
C
      CALL initl(ier)
      IF ( ier.NE.0 ) STOP
C
C  INTERPRET ACTION
C
      idoo = 0
      kk = icardp
      CALL decip(icard,jcom,icardp)
C
C  USER NEED TO BE PROMPTED FOR ACTION ?
C
      IF ( jcom(1).EQ.0 ) THEN
C
C  YES.  PROMPT FOR ACTION
C
         WRITE (nprnt,99001)
99001    FORMAT (/,' ENTER: MAPS, MASTER, WORK, OR <MAPNAME>'\)
         CALL readin(icard,lenicd,icardp,iend)
         CALL decip(icard,jcom,icardp)
         lines = lines + 2
      ENDIF
C
C  INTERPRET ACTION
C
      IF ( jcom(1).NE.0 .OR. idoo.NE.0 ) THEN
         IF ( jcom(1).EQ.0 .AND. idoo.NE.0 ) GOTO 99999
         IF ( .NOT.(mcomp(jcom,'WORK',4,ier)) ) THEN
            IF ( .NOT.(mcomp(jcom,'MAST',4,ier)) ) THEN
               IF ( .NOT.(mcomp(jcom,'MAPS',4,ier)) ) THEN
C
C  DEVICE NAMES FOR DISPLAY ON COLOR CRT'S   <not applicable in micro>
C
C 1600   CONTINUE
C        CALL OPENF(JCHAN,'PSEUDO.DT',2,256,2,IER)
C        IF(IER.NE.1) GO TO 80000
C        CALL PDLIST(JCHAN,IER)
C        CALL CLOSE(JCHAN,IER)
C        CALL READY(LINES)
C        GO TO 90000
C
C  COLOR DEFINTION NAMES
C
C 1800   CONTINUE
C        CALL LSTCOL(LINES,IER)
C        CALL CPLAC(LINES)
C        LINES=0
C        GO TO 90000
C
C  CALL MAP NAME DESCRIBING ROUTINE
C
                  idoo = 2
                  icardp = kk
                  CALL desc(lines,ier)
                  GOTO 99999
               ENDIF
            ENDIF
         ENDIF
      ENDIF
C
C  LIST MAPS IN MASTER AND/OR WORK
C
      isw = -1
      idoo = 2
      CALL decip(icard,icom,icardp)
      lines = 0
      IF ( mcomp(jcom,'MAP',3,ier) ) isw = 0
      IF ( mcomp(jcom,'MAS',3,ier) ) isw = 1
      IF ( mcomp(jcom,'WO',2,ier) ) isw = 2
      IF ( mcomp(jcom,'PO',2,ier) ) isw = 2
C
      IF ( dbnmap(1).NE.0 .AND. isw.LE.0 .OR. isw.EQ.1 )
     &      CALL lstdir(ibuff,128,map,25,icom,1,lines,idoo,istop)
      IF ( istop.NE.1 ) THEN
C
         IF ( isw.LE.0 .OR. isw.EQ.2 )
     &         CALL lstdir(ibuff,128,map,25,icom,2,lines,idoo,istop)
      ENDIF
C
C  ERROR RETURNS
C
C80000   WRITE(NPRNT,98000)  IER
C98000   FORMAT(' *LIST*   ERROR OPENING PSEUDO.DT  ERROR',I5)
C        LINES=LINES+2
C        GO TO 90000
C
C  JUMP OUT
C
99999 CONTINUE
      END
