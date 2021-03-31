        SUBROUTINE BTEMPLT(IARR,IEDIT,ichan,IER)
C
C **************************************************************
C
C            S U B R O U T I N E    B T E M P L T
C FUNCTION:
C            THIS ROUTINE BUILDS A HEADER TO A DIGITIZED MAP FILE.
C PARAMETERS:
C            IARR      IN  - THE ARRAY IN WHICH HEADER IS BUILT.
C            IEDIT     IN  - 0-GENERATE NEW HEADER 1-USE HEADER SENT IN IARR
C            IER       OUT - THE ERROR RETURN FOR WRITING TO, READING,
C                            OPENING, OR CLOSING A FILE,
C                            IER =  NOT 0, IF AN ERROR,
C                            IER = 0, IF NO ERROR.
C ROUTINES
C CALLED:    CLEAR,READIN,INUM,PRNHED,PROIN,PROVW.
C
C ***************************************************************
C
$Include:'IO.inc'
        INTEGER STRING(80)
        DIMENSION IARR(256),IANS(80),IPROJ(64),IT(2),iar(5),isu(15)
        DOUBLE PRECISION BUFIO(15)
        LOGICAL MCOMP
        EQUIVALENCE (SCALE,IT(1)),(IPROJ(5),BUFIO(1))
C
  1     CONTINUE
        IBLANK=2H
        ICHRN=0
        IER=0
C
C  SEE IF THEY WANT TO START FROM SCRATCH OR EDIT EXISTING HEADER
C
       IF (IEDIT .EQ. 1) GO TO 10
       DO 5 I=2,57
         IARR(I) = IBLANK
   5   CONTINUE
       IARR(64) = 1
       IARR(65) = 1
       IARR(66) = 0
       IARR(67) = 1
       IARR(77) = 77
       SCALE = 1.0
       IARR(127) = IT(1)
       IARR(128) = IT(2)
C
       CALL CLEAR(IARR(129),128)
C
C  INITIALIZE RESERVED WORDS AND WORDS THAT NEED TO BE REASSIGNED
C
  10    IARR(1) = 0
        IARR(50) = 0
        CALL CLEAR(IARR(58),4)
        CALL CLEAR(IARR(68),8)
        IARR(76)=0
        CALL CLEAR(IARR(78),48)
        DO 15 I=1,64
          IPROJ(I) = IARR(192+I)
  15    CONTINUE
C
C  GET DIGITIZER NAME
C
  20    CONTINUE
        WRITE(NPRNT,3020) (IARR(I),I=2,11)
 3020   FORMAT(/' ENTER SOURCE [',10A2,']'\)
        IANS(1)=0
        CALL READIN(STRING,80,ISTART,ISTOP)
        IF(STRING(1).EQ.0) GO TO 60
        CALL BLFILL(STRING,80)
        CALL PACKC(STRING,IANS,80)
           DO 40 I=1,10
              IARR(I+1)=IANS(I)
   40      CONTINUE
C
C GET CREATION DATE OF DIGITIZED MAP
C
   60   CONTINUE
        WRITE(NPRNT,3060) (IARR(I),I=12,15)
 3060   FORMAT(' ENTER  CREATION DATE  [',4A2,']'\)
        CALL READIN(STRING,80,ISTART,ISTOP)
        IF(STRING(1).EQ.0) GO TO 100
        CALL BLFILL(STRING,80)
        CALL PACKC(STRING,IANS,80)
         DO 80 I=1,4
            IARR(I+11)=IANS(I)
   80    CONTINUE
C
C  GET STUDYAREA NAME
C
  100   CONTINUE
        WRITE(NPRNT,3100) (IARR(I),I=16,19)
 3100   FORMAT(' ENTER  STUDYAREA NAME [',4A2,']'\)
        CALL READIN(STRING,80,ISTART,ISTOP)
        IF(STRING(1).EQ.0) GO TO 140
        CALL BLFILL(STRING,80)
        CALL PACKC(STRING,IANS,80)
           DO 120 I=1,4
              IARR(I+15)=IANS(I)
  120      CONTINUE
C
C GET 60 CHARACTER DESCRIPTION
C
  140   if(iarr(20).le.8224)then
         write(*,3141)
 3141   format(' ENTER DESCRIPTION (up to 60 char.)'\)
        else
        WRITE(NPRNT,3140) (IARR(I),I=20,49)
 3140   FORMAT(' ENTER DESCRIPTION (up to 60 char.)',/,' [',30A2,']'\)
        endif
         CALL READIN(STRING,80,ISTART,ISTOP)
         IF(STRING(1).EQ.0) GO TO 180
         CALL BLFILL(STRING,80)
         CALL PACKC(STRING,IANS,80)
         DO 160 I=1,30
              IARR(I+19)=IANS(I)
  160      CONTINUE
C
C GET MAP VINTAGE
C
  180   CONTINUE
        WRITE(NPRNT,3180) (IARR(I),I=51,52)
 3180   FORMAT(' ENTER  MAP VINTAGE  [',2A2,']'\)
        CALL READIN(STRING,80,ISTART,ISTOP)
        IF(STRING(1).EQ.0) GO TO 220
        CALL BLFILL(STRING,80)
        CALL PACKC(STRING,IANS,80)
           DO 200 I=1,2
              IARR(I+50)=IANS(I)
  200      CONTINUE
C
C GET PROJECTION DESCRIPTION
C
  220   CONTINUE
        WRITE(NPRNT,3220) (IARR(I),I=53,57)
 3220   FORMAT(' ENTER MAP PROJECTION DESCRIPTION [',5A2,']'\)
        CALL READIN(STRING,80,ISTART,ISTOP)
        IF(STRING(1).EQ.0) GO TO 280
        CALL BLFILL(STRING,80)
        CALL PACKC(STRING,IANS,80)
           DO 240 I=1,5
              IARR(I+52)=IANS(I)
  240      CONTINUE
C
C ASSIGN NUMBER  & GET # OF SUBJECTS
C
  280   CONTINUE
        ITEMP=0
        if(iarr(65).eq.1)iarr(65)=0
        WRITE(NPRNT,3260) IARR(65)
 3260   FORMAT(' ENTER NUMBER OF SUBJECTS [',I4,']'\)
        IANS(1)=0
        CALL READIN(IANS,80,ISTART,istop)
C        IF(IANS(1).EQ.ICHRN)GO TO 320
        CALL INUM(IANS(1),ITEMP,ISTART)
        IF(ITEMP.EQ.0) then
        write(*,*)' Searching Input file - counting subjects ',
     + '-Please wait'
         rewind(ichan)
3261     read(ichan,3262,end=3265)ktemi,(iar(il),il=1,5),
     + (isu(il),il=1,15),numr
3262   format(i5,5a2,15a2,5x,i5)
3264   format(2f11.2)
          do 3266 il=1,numr
3266      read(ichan,3264,end=3265)xd,yd
          itemp=itemp+1
          go to 3261
3265    rewind(ichan)
        endif
        IF(ITEMP.LE.0) GO TO 280
        IF(ITEMP.LE.2750) GO TO 300
        WRITE(NPRNT,3280)
 3280   FORMAT(' TOO MANY SUBJECTS...MUST BE LESS THAN 2750')
        GO TO 280
C
C  SET ESTIMATED & ACTUAL CATS,ESTIMATED & ACTUAL SUBS
C
300    IARR(65)=ITEMP
       IARR(67)=ITEMP
C
C  GET FEET OR METERS
C
 320    CONTINUE
        WRITE(NPRNT,3200) IARR(77)
 3200   FORMAT(' ARE COORDINATES IN FEET OR METERS [',A1,']'\)
        CALL READIN(STRING,80,ISTART,ISTOP)
        IF(STRING(1).EQ.ICHRN) GO TO 340
        IARR(77)=0
        IF(MCOMP(STRING(1),'F',1,IER)) IARR(77)=70
        IF(MCOMP(STRING(1),'M',1,IER))  IARR(77)=77
        IF(IARR(77).EQ.0) GO TO 320
C
C  GET COORDINATE MULTIPLICATIVE SCALE
C
 340    CONTINUE
        IT(1)=IARR(127)
        IT(2)=IARR(128)
        WRITE(NPRNT,3400) SCALE
 3400   FORMAT(' ENTER COORDINATE SCALE FACTOR [',F7.4,']'\)
        CALL READIN(STRING,80,ISTART,ISTOP)
        IF(STRING(1).EQ.0) GO TO 360
        CALL FNUM(STRING,80,SCALE,ISTART)
        IF(SCALE.LT.0.0) GO TO 320
        IARR(127)=IT(1)
        IARR(128)=IT(2)
C
C  GET NEW PROJECTION PARAMETERS
C
  360   CONTINUE
        CALL PROVW(NPRNT,IPROJ(1),IPROJ(2),IPROJ(3),IPROJ(4),BUFIO(1))
        If(iproj(1).ne.0) then
          WRITE(NPRNT,3600)
 3600   FORMAT(/,' DO YOU WISH TO CHANGE THE PROJECTION PARAMETERS',
     +           ' (HEADER ONLY) [N] ',\)
        CALL READIN(iget,1,ISTART,ISTOP)
         else
        if(iget.eq.ichar('Y').or.iget.eq.ichar('y').or.iproj(1).eq.0)
     +  then
        CALL PROIN(IOIN,NPRNT,2,IPROJ(1),IPROJ(2),IPROJ(3),IPROJ(4),
     +BUFIO(1))
         endif
       endif
        DO 365 I=1,64
          IARR(192+I) = IPROJ(I)
  365   CONTINUE
C
C      THE ROUTINE EXIT.
C
  380   CONTINUE
        WRITE(NPRNT,3800)
 3800   FORMAT(/,' IS THIS HEADER INFORMATION CORRECT [Y] '\)
        CALL READIN(STRING,80,ISTART,ISTOP)
        IF(MCOMP(STRING(1),'N',1,IER)) GO TO 10
C
        RETURN
        END
