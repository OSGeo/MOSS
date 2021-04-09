      SUBROUTINE PLPARM(ICARD,K,BORD,COLOR,TICS,DINC,FAST,IDISP,IER)
C *********************************************************************
C
C                S U B R O U T I N E   P L P A R M
C
C  Purpose: This routine checks for parameters for the plot command.
C
C  Parameters:
C     Name       Use        Type         Dimension       Description
C     ----       ---        ----         ---------   ------------------
C
C     ICARD       I          INT         80          USER COMMAND STRING
C     K           I          INT         2/4 BYTES   POINTER INTO ICARD
C     BORD        O          LOG         2/4 BYTES   TRUE= DRAW BORDER
C                                                    FALSE= NO BORDER
C     COLOR       O          LOG         2/4 BYTES   TRUE= COLOR
C                                                    FALSE= NO COLOR
C     TICS        O          LOG         2/4 BYTES   TRUE= TICS
C                                                    FALSE= NOTICS
C     DINC        O          REAL        4 BYTES     TIC INCREMENT
C     FAST        O          LOG         2/4BYTES    FALSE= DATABASE PLOT
C                                                    TRUE= GRAPHICS PLOT
C     IDISP       O          INT         10          CONTAINS GRAPHICS DISPLAY
C                                                    FILE NAME IF FAST IS TRUE
C     IER         O          INT         2/4 BYTES   ERROR RETURN
C
C  Calls: DECIP, MCOMP
C **********************************************************************
$Include:'IO.inc'
C
      DIMENSION ICARD(80),ICOM(10),IDISP(10)
C
      LOGICAL MCOMP,BORD,FAST,TICS,COLOR
C
C
        COLOR=.FALSE.
        FAST= .FALSE.
        TICS= .FALSE.
        BORD= .TRUE.
        DINC= 0.0
        IOPT= 0
        IDISP(1) = 0
C
C  STORE VALUE OF K
C
  5   CONTINUE
      KK=K
C
C  CHECK FOR SOMETHING
C
      CALL DECIP(ICARD,ICOM,K)
      IF(MCOMP(ICOM(1),'-',1,IER)) GO TO 7
      IF(ICOM(1).NE.0) GO TO 10
C
C  NOTHING.  RESTORE K AND RETURN
C
  7   CONTINUE
      K=KK
      GOTO 999
C
C  SOMETHING.  DETERMINE WHAT IT IS
C
  10  CONTINUE
      IF(MCOMP(ICOM(1),'N',1,IER)) GO TO 100
      IF(MCOMP(ICOM(1),'NO',2,IER)) GO TO 100
      IF(MCOMP(ICOM(1),'NOB',3,IER)) GO TO 100
      IF(MCOMP(ICOM(1),'NOBO',4,IER)) GO TO 100
      IF(MCOMP(ICOM(1),'NOBOR',5,IER)) GO TO 100
      IF(MCOMP(ICOM(1),'NOBORD',6,IER)) GO TO 100
      IF(MCOMP(ICOM(1),'NOBORDE',7,IER)) GO TO 100
      IF(MCOMP(ICOM(1),'NOBORDER',8,IER)) GO TO 100
      IF(MCOMP(ICOM(1),'T',1,IER)) GO TO 200
      IF(MCOMP(ICOM(1),'TI',2,IER)) GO TO 200
      IF(MCOMP(ICOM(1),'TIC',3,IER)) GO TO 200
      IF(MCOMP(ICOM(1),'TICS',4,IER)) GO TO 200
      IF(MCOMP(ICOM(1),'G',1,IER)) GO TO 300
      IF(MCOMP(ICOM(1),'GR',2,IER)) GO TO 300
      IF(MCOMP(ICOM(1),'GRA',3,IER)) GO TO 300
      IF(MCOMP(ICOM(1),'GRAP',4,IER)) GO TO 300
      IF(MCOMP(ICOM(1),'GRAPH',5,IER)) GO TO 300
      IF(MCOMP(ICOM(1),'GRAPHI',6,IER)) GO TO 300
      IF(MCOMP(ICOM(1),'GRAPHIC',7,IER)) GO TO 300
      IF(MCOMP(ICOM(1),'GRAPHICS',8,IER)) GO TO 300
      IF(MCOMP(ICOM(1),'D',1,IER)) GO TO 400
      IF(MCOMP(ICOM(1),'DE',2,IER)) GO TO 400
      IF(MCOMP(ICOM(1),'DEV',3,IER)) GO TO 400
      IF(MCOMP(ICOM(1),'DEVI',4,IER)) GO TO 400
      IF(MCOMP(ICOM(1),'DEVIC',5,IER)) GO TO 400
      IF(MCOMP(ICOM(1),'DEVICE',6,IER)) GO TO 400
      IF(MCOMP(ICOM(1),'C',1,IER)) GO TO 500
      IF(MCOMP(ICOM(1),'CO',2,IER)) GO TO 500
      IF(MCOMP(ICOM(1),'COL',3,IER)) GO TO 500
      IF(MCOMP(ICOM(1),'COLO',4,IER)) GO TO 500
      IF(MCOMP(ICOM(1),'COLOR',5,IER)) GO TO 500
      IF (IOPT .EQ. 0) THEN
        WRITE(NPRNT,3040)
3040    FORMAT(' Unprompted options are:',
     +  ' NOBORDER, COLOR n, GRAPHICS file, or DEVICE file')
        IER=-1
        GO TO 999
      ENDIF
      goto 5
C
C  NOBORDER
C
 100  CONTINUE
      BORD = .FALSE.
      IOPT=IOPT+1
      GO TO 5
C
C  TICS IS TRUE SO PICK UP THE INCREMENT
C
 200   CONTINUE
C      CALL DECIP(ICARD,ICOM,K)
C      CALL FNUM(ICOM,10,DINC,I)
C      IF(DINC.LE.0.0) GO TO 900
C      TICS = .TRUE.
       GO TO 5
C
C  GRAPHICS DISPLAY FAST FILE
C
 300  CONTINUE
      CALL DECIP(ICARD,IDISP,K)
      IF (IDISP(1) .EQ. 0) GO TO 920
      FAST = .TRUE.
      IOPT=IOPT+1
      GO TO 5
C
C check for change of device
C
 400  call DEVGET(icard,k)
      IOPT=IOPT+1
      go to 5
C
C Check for color association override
C
 500  CONTINUE
      COLOR=.TRUE.
      IOPT=IOPT+1
      GOTO 5
C
C  ERROR PROCESSING
C
c 900  WRITE(NPRNT,3000)
c 3000 FORMAT(' Value for TIC increment must be greater than 0')
c      GO TO 999
C
 920  WRITE(NPRNT,3020)
 3020 FORMAT(' MUST SPECIFY A GRAPHICS DISPLAY NAME AFTER "GRAPHICS".')
      GO TO 999
C
C  ALL DONE
C
 999  CONTINUE
      RETURN
      END
