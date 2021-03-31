C
C           S U B R O U T I N E    L S T D I R
C
C  FUNCTION:  LIST THE MAPS STORED IN A PARTICULAR MOSS MAP NAMES
C              FILE.  NAMES are LISTED IN ASCENDING ALPHA ORDER
C PARAMETERS:
C             IARR,JDIM IN  - WORK SPACE ARRAY.
C             ICOM,LEN  IN  - THE MAP NAME STRING, ICOM IS THE KEY TO SEARCH
C                             THE TABLE, LEN IS THE STRING SIZE.
C             IPLATE    IN  - TEMPLATE TO MATCH IF REQUIRED
C             ICHAN     IN  - HASH TABLE CHANNEL.
C             LINES     IN  - THE NUMBER OF OUTPUT LINES FROM ROUTINE.
C             IDO       IN  - 1,NORMAL  2-LIST BY TYPE
C             ISTOP     OUT - 1 CONTINUE  0 STOP
C ROUTINES
C CALLED:     GETMAP,READY,TEMPL,GTNAM.
C
C ********************************************************************
C
        SUBROUTINE LSTDIR(IARR,JDIM,ICOM,LEN,IPLATE,
     +   ICHAN,LINES,IDO,ISTOP)
        COMMON /IO/     NPRNT,IOIN
$Include:'dnames.inc'
        common/dbmap/dbnmap(2)
        COMMON /DIRSY/  LASTC,LASTBK,IWTRN,KDIR(256)
        INTEGER IARR(JDIM),ICOM(LEN),NAME(16),FIRST,IPLATE(10),dbnmap
C
C  CHECK ACTION
C
        KMAP=1
        ICHEK=0
        IF (IDO.GT.2) ICHEK=IDO-3
        IF(IDO.GT.2) IDO=2
C
C  SEE IF TEMPLATE BEING USED  <check for + sign>
C
        IFLAG = 0
        DO 14 I=1,10
          IF (IPLATE(I) .EQ. 43) IFLAG = 1
  14    CONTINUE
C
C  MAKE SURE MASTER IS OPEN
C
       IF (ICHAN .EQ. 1  .AND. MAST(1) .EQ. 0) GO TO 90
C
C  GET NUMBER OF MAPS BUT MAKE SURE TO READ THE BLOCK
C
        nrecs=dbnmap(ichan)
c       if(nrecs.eq.0)go to 16
c        LASTC = -1
c        IWTRN = -1
c        CALL GETMAP(ICHAN,1,NAME,IER)
c        NRECS = NAME(16)-1
c        IF (NRECS .LT. 1  .OR.  IER .NE. 0) GO TO 90
C
C  WRITE INTIAL MESSAGE
C
  16   IF (ICHAN .EQ. 1) WRITE(NPRNT,1001) NRECS,(MAST(I),I=1,25)
1001   FORMAT(/,1X,I5,' MAPS IN MASTER PROJECT: ',25A1,/)
       IF (ICHAN .NE. 1) WRITE(NPRNT,1002) NRECS,(IWORK(I),I=1,25)
1002   FORMAT(/,1X,I5,' MAPS IN WORK PROJECT: ',25a1,/)
       LINES = LINES + 3
C
        NROWS=(NRECS/4)
        LFTOV=NRECS-(NROWS*4)
        IFULL=NROWS*4
        L = 0
C
        IF(IFULL.EQ.0) GO TO 50
C
        I=0
  30    CONTINUE
        IF(I.GT.IFULL) GO TO 35
        L=0
        KNT=0
  20    CONTINUE
        I=I+1
        IF(I.GT.IFULL) GO TO 35
        KMAP=KMAP+1
        CALL GETMAP(ICHAN,KMAP,NAME,IER)
        IF(ICHEK.GT.0.AND.NAME(10).NE.ICHEK) GO TO 20
        IDTYPE = NAME(10)
C**     UNPACK AND NULL FILL
C
        do 5 ii=1,10
    5   icom(ii)=name(ii)
        call unpack(icom,20)
C
        IF(IFLAG.EQ.0) GO TO 15
        CALL TEMPL(ICOM,IPLATE,MAT)
        IF(MAT.EQ.0) GO TO 20
  15    CONTINUE
        KNT=KNT+1
        DO 10 K=1,10
        L=L+1
        IARR(L)=ICOM(K)
C  NOTE.  32 IS A BLANK (SPACE)
        IF(IARR(L).EQ.0) IARR(L)=32
  10    CONTINUE
         IF(IDO.EQ.2) L=L+1
        IF(IDO.EQ.2) IARR(L)=IDTYPE
        IF(KNT.EQ.4) GO TO 21
        GO TO 20
  21    CONTINUE
        IF(IDO.LT.2) WRITE(NPRNT,1000) (IARR(J),J=1,L)
 1000   FORMAT(4(10a1,5X))
        IF(IDO.EQ.2) WRITE(NPRNT,1005) (IARR(J),J=1,L)
 1005   FORMAT(1X,4(10a1,I2,4X))
        CALL CONTIN(LINES,ISTOP)
        IF(ISTOP.EQ.1) GO TO 90
        GO TO 30
  35    CONTINUE
        IF(KNT.GT.0.AND.KNT.LT.4) WRITE(NPRNT,1005) (IARR(I),I=1,L)
C
  50    CONTINUE
C
        IF(LFTOV.EQ.0) GO TO 90
C
        L=0
        DO 70 I=1,LFTOV
        KMAP=KMAP+1
        CALL GETMAP(ICHAN,KMAP,NAME,IER)
        IF(ICHEK.GT.0.AND.NAME(10).NE.ICHEK) GO TO 70
        IDTYPE=NAME(10)
C**     UNPACK AND NULL FILL
       DO 63 II=1,10
         ICOM(II) = name(ii)
63     CONTINUE
       call unpack(icom,20)
C
C
        IF(IFLAG.EQ.0) GO TO 65
        CALL TEMPL(ICOM,IPLATE,MAT)
        IF(MAT.EQ.0) GO TO 70
  65    CONTINUE
        DO 60 J=1,10
        L=L+1
        IARR(L)=ICOM(J)
        IF(IARR(L).EQ.0) IARR(L)=32
  60    CONTINUE
        IF(IDO.EQ.2) L=L+1
        IF(IDO.EQ.2) IARR(L)=IDTYPE
        IF(KMAP.EQ.NRECS+1) GO TO 75
  70    CONTINUE
  75    CONTINUE
        IF(L.GT.0.AND.IDO.LT.2) WRITE(NPRNT,1000) (IARR(I),I=1,L)
        IF(L.GT.0.AND.IDO.EQ.2) WRITE(NPRNT,1005) (IARR(I),I=1,L)
        IF(L.GT.0) CALL CONTIN(LINES,ISTOP)
        IF (ISTOP.EQ.1) GO TO 90
C
  90    CONTINUE
        RETURN
        END
