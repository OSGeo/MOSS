C ************************************************************
C
C        P R O G R A M    FREE
C
C FUNCTION:   PROGRAM ALLOWS THE USER TO DE-ACTIVATE ACTIVATED
C             DATA SETS.
C
C ROUTINES
C CALLED:    DECIP,ADSDEL,CLEAR,CPLAC,READIN,DAGET,OUTCM,INITL
C
C ************************************************************
$Include:'DBCHAN.inc'
$Include:'IO.inc'
$Include:'TYPE.inc'
$Include:'WHMAP.inc'
$Include:'STRNG.inc'
       COMMON /ATTFET/ KAT(86),LBAT
       COMMON /PNTFET/ NPOINT
C
       LOGICAL MCOMP
C
       INTEGER IREC(31),IALL(10),LISTFRE(40)
C
C
       IEND = 80
       LINES=1
C
C   General initialization, reading common area in from incm
      CALL INITL(IER)
      IF ( IER .NE. 0 ) GO TO 8200
C
C  Does user wish to free everything ?
       L=ICARDP
       LINES=1
       CALL DECIP(ICARD,IALL,ICARDP)
C
C  USER NEED PROMPTING ?
C
       IF ( IALL(1) .EQ. 0 ) THEN
         WRITE(NPRNT,1111)
 1111 FORMAT(' Enter  ALL (to free entire table)  or active map ID '\)
         CALL READIN(ICARD,LENICD,ICARDP,IEND)
         L=ICARDP
         LINES=LINES+3
C
C Interpret action
         CALL DECIP(ICARD,IALL,ICARDP)
       ENDIF
C
C  Check if freeing ALL was requested
       IF ( MCOMP(IALL,'AL',2,IER) ) GO TO 200
       ICARDP = L
C
C  GET I.D.'S OF DATA SETS TO FREE
C
       CALL DAGET(ICARD,ICARDP,LISTFRE,40,MACTS)
       IF( MACTS .EQ. 0 ) THEN
C
C PROMPT FOR ACTIVE MAPS TO FREE
C
          WRITE(NPRNT,1001)
 1001     FORMAT(' ENTER ACTIVE MAPS TO FREE')
          CALL READIN(ICARD,LENICD,ICARDP,IEND)
          CALL DAGET(ICARD,ICARDP,LISTFRE,40,MACTS)
          LINES=LINES+3
          IF(MACTS.LE.0) GO TO 8100
       ENDIF
C
C  OPEN DESCRIBE.DT
C
       CALL OPENF(ICDCDT,IDESDT,2,62,2,IER)
       IF ( IER .NE. 0 ) GO TO 950
C
C  ORDER ACTIVE MAP I.D.'S IN ASCENDING ORDER
C
       IF( MACTS .GT. 1 ) THEN
          JN=MACTS-1
          DO 22 I=1,JN
            JL=I+1
            DO 21 J=JL,MACTS
              IF(LISTFRE(I)-LISTFRE(J)) 21,21,24
  24          IT=LISTFRE(I)
              LISTFRE(I)=LISTFRE(J)
              LISTFRE(J)=IT
  21        CONTINUE
  22      CONTINUE
       ENDIF
C
C  LOOP THROUGH MACTS TIMES, FREEING DATA SETS
C
       KSTRCT=NSTRCT
       DO 30 I=MACTS,1,-1
         CALL ADSDEL(IER,LISTFRE(I),INDEX,NPNTS)
  30   CONTINUE
C
C  UPDATE TYPE TABLE AND SELECTION CRITERIA FILE
C
       DO 60 I=MACTS,1,-1
         ID=LISTFRE(I)
         ISTART=(ID/7)+1
         ISTOP=(KSTRCT/7)-1
         IF (ISTART .GT. ISTOP) GO TO 50
         DO 40 J=ISTART,ISTOP
           IDATYP(J)=IDATYP(J+1)
           KAT((J*2)-1) = KAT((J+1)*2-1)
           KAT((J*2)  ) = KAT((J+1)*2  )
           READ(ICDCDT,REC=J+1,IOSTAT=IER,ERR=40) IREC
           WRITE(ICDCDT,REC=J,IOSTAT=IER,ERR=40) IREC
  40     CONTINUE
  50     KSTRCT=KSTRCT-7
  60   CONTINUE
       CLOSE(ICDCDT)
C
C  RESET NUMBER OF ACTIVE AND LAST POSITION USED IN POINT.DT AND DESCRIBE.FA
C
       NACTS=NACTS-MACTS
       IF (NACTS .GE. 1) THEN
          ID =((NACTS-1)*7) + 1
          I = ISTRCT(ID) + ISTRCT(ID+1) - 1
          NPOINT = I
          GO TO 90000
       ENDIF
C
C  FREE ENTIRE STRUCTURE TABLE
C
 200   CONTINUE
       CALL CLEAR(ISTRCT,300)
       CALL CLEAR(IDATYP,40)
       CALL CLEAR(KAT,86)
       NPOINT=0
       NSTRCT=0
       NACTS=0
       GO TO 90000
C
C  ERROR RETURNS
C
 8100  WRITE(NPRNT,98100)
98100  FORMAT(' ILLEGAL ACTIVE I.D. ')
       LINES=LINES+2
       GO TO 90000
C
 8200  WRITE(NPRNT,98200)
98200  FORMAT(' *ACTIVE*   ERROR FROM INITL, TROUBLE WITH INTERPROC')
       GO TO 90000
C
 950   WRITE(NPRNT,3050) IER
 3050  FORMAT(' *FREE* COULD NOT OPEN DESCRIBE.DT  ERROR',I5)
       LINES=LINES+2
       GO TO 90000
C
C  JUMP OUT
C
90000  CONTINUE
C
       CALL TYPESC
C?       CALL CPLAC(LINES)
C?       CALL POSTF
       CALL OUTCM
       END
