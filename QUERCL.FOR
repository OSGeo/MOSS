C***********************************************************************
C
C     NAME:  SUBROUTINE QUERCL
C
C     FUNCTION:  PERFORMS QUERY COMMAND WHICH COMPUTES THE ROW AND COLUMN
C                LOCATION OF ANY GIVEN POINT FROM A BINARY, DISCRETE, OR
C                CONTINUOUS MAP WHICH HAS BEEN IDENTIFIED BY POSITIONING
C                THE CROSSHAIRS ON A GRAPHIC REPRESENTATION OF THAT MAP,
C                AND THEN OUTPUTS INFORMATION ABOUT THAT POINT INCLUDING
C                THE ROW AND COLUMN NUMBER AND THE CELL VALUE, DESCRIPTION,
C                AND FREQUENCY.
C
C     PARAMETERS:  NAME     USE     TYPE     DIMENSION     RANGE
C                  -------------------------------------------------
C                  XX        I      R*4                    +REAL
C                  X POSITION OF CELL OF INTEREST
C                  -------------------------------------------------
C                  YY        I      R*4                    +REAL
C                  Y POSITION OF CELL OF INTEREST
C                  -------------------------------------------------
C                  IDD       I      I*2                    +INTEGER
C                  ACTIVE MAP TABLE ID
C                  -------------------------------------------------
C                  IER       O      I*2                     INTEGER
C                  ERROR INDICATOR
C                  -------------------------------------------------
C
C*************************************************************************
C
      SUBROUTINE QUERCL (xx,yy,IDD,loop,ifound,IER)
C
      COMMON /CHANL/ IO(40)
      COMMON /DSBUF/ MLEN,IDATA(8192)
      INTEGER IHEAD(256),JBUFF(128),MAP(25),IVALU(4),ITEMP(2)
      integer mapname(25),iwrk(2)
      REAL RVALU(2),OLDWND(4),CELL,XSIZE,YSIZE
      EQUIVALENCE (IVALU(1),RVALU(1)), (ITEMP(1),RTEMP)
      EQUIVALENCE (IHEAD(68),OLDWND(1)),(IHEAD(80),YSIZE)
      EQUIVALENCE (IHEAD(82),XSIZE)
      EQUIVALENCE (IHEAD(84),BITSET)
      DATA SMVAL /.00001/
C
      MLEN =8193
      ICHAN = 20
      ICHCL = 21
      ICVC  = 22
      ICAD  = 23
C
C.....OPEN, READ, AND CLOSE CELL MAP HEADER .....
C
      CALL GTNAM(IDD,5,2h  ,MAP,IER)
      call adddir(map,mapname)
      CALL DSOPN(ICHAN,MAPname,1,1,1,256,256,IER)
      IF (IER .NE. 0) GO TO 85020
      CALL DSIN(ICHAN,IHEAD,1.0,IER)
      IF (IER .NE. 0) GO TO 85040
      CALL DSCLS(ICHAN,IER)
C
C.....RETRIEVE PARAMETERS .....
      NROWS = IHEAD(60)
      NCOLS = IHEAD(61)
      ICTYP = IHEAD(62)
      NSUBS = IHEAD(67)
      call byteit(ihead(185),iwrk)
      len=iwrk(1)
      IBW = 1
      IF (LEN .LT. 16) IBW = 2
      IF (IBW .EQ. 1 ) LEN = LEN/16
      NWORD = IHEAD(191)/2
C
C.....OPEN THE CELL DATA FILE .....
      CALL GTNAM(IDD,ICTYP,2h  ,MAP,IER)
      call adddir(map,mapname)
      CALL DSOPN(ICHCL,MAPname,1,1,IBW,LEN,128,IER)
      IF (IER .NE. 0) GO TO 85020
C
C......IF A DISCRETE MAP THEN OPEN THE VALUE,COUNT AND DESCRIPTOR FILES .....
       IF (ICTYP .NE. 7) GO TO 1000
       CALL GTNAM(IDD,11,2h  ,MAP,IER)
       CALL DSOPN(ICVC,MAP,1,1,1,4,256,IER)
       IF (IER .NE. 0) GO TO 85020
C
       IF (NWORD .LE. 0) GO TO 1000
       CALL GTNAM(IDD,12,2h  ,MAP,IER)
       CALL DSOPN(ICAD,MAP,1,1,1,NWORD,256,IER)
       IF (IER .NE. 0) GO TO 85020
C
C......GET COORDINATES OF QUERIED POINT .......
 1000  loop=loop+1
       if(loop.gt.1)go to 1100
C......IF GOOD X,Y PAIR USE IT !!!
       IF (XX .NE. 0. .AND. YY .NE. 0.) GO TO 1100
       IF (ABS(XX) .GT. SMVAL .AND. ABS(YY) .GT. SMVAL) GO TO 1100
C......PROMPT AND GET NEXT LOCATION
C       WRITE(*,1003)
C 1003  FORMAT(' POINT TO CELL, SPACE or RETURN will REPEAT')
C       CALL FLSH
       CALL VCURSR (IC,XX,YY)
C
C......SEE IF THEY SAID DO IT AGAIN R(EPEAT) .....
c      IF ((IC .NE. 32) .AND. (IC .NE. KCHS(10)) .AND.
c     +    (IC .NE. KCHS(13))) GO TO 90000
       IF (IC .NE. 32 .AND. IC .NE. 10 .AND. IC .NE. 13) GO TO 90000
C......PLOT 'X' TO MARK POSITION OF QUERIED POINT .......
       CALL MOVEA (XX,YY)
       CALL MOVREL (-4, -8)
       CALL DRWREL ( 8, 16)
       CALL MOVREL (-8,  0)
       CALL DRWREL ( 8,-16)
C
C......COMPUTE ROW/COLUMN LOCATION OF QUERIED CELL ......
 1100  IROW = INT ((OLDWND(4) - YY) / YSIZE) + 1
       ICOL = INT ((XX - OLDWND(1)) / XSIZE) + 1
        IF (IROW.LT.1 .OR. IROW.GT.NROWS .OR.
     +      ICOL.LT.1 .OR. ICOL.GT.NCOLS ) GO TO 85060
       CELL = (FLOAT(IROW-1) * FLOAT(NCOLS)) + FLOAT(ICOL)
       if(loop.gt.1)go to 989
C
C......PLOT OUTLINE OF CELL BORDER ......
       X = (FLOAT(ICOL-1) * XSIZE) + OLDWND(1)
       Y = (FLOAT(NROWS-IROW) * YSIZE) + OLDWND(3)
       CALL MOVEA (X,Y)
       CALL DRAWA (X+XSIZE,Y)
       CALL DRAWA (X+XSIZE,Y+YSIZE)
       CALL DRAWA (X,Y+YSIZE)
       CALL DRAWA (X,Y)
       call flsh
C
C......WRITE THE POSITION IN METERS AND THE ROW & COLUMN .....
       write(*,*)char(27),char(12)
989    WRITE (*,96850) (map(ik),ik=1,12),YY,XX,IROW,ICOL
96850  FORMAT (/,' MAP:',12a2,/' NORTHING ',F11.2,' EASTING   ',
     + f11.2,/,' ROW      ',  I11,' COLUMN    ',  I11)
C
C......BRANCH ACCORDING TO TYPE ......
       GO TO (68600,68700,68800), ICTYP-5
C
C......PROCESS TYPE 6 MAP ......
68600  CONTINUE
C...........READ CELL VALUE FROM OLDMAP ......
            CALL DSIN (ICHCL,IVALU(1),CELL,IER)
            IF (IER .NE. 0)  GO TO 85040
C...........SET VALUE & FREQUENCY OF VALUE ......
            RVALU(2) = BITSET
            IF (IVALU(1) .EQ. 0)
     1           RVALU(2) = FLOAT(NROWS)*FLOAT(NCOLS) - RVALU(2)
C...........OUTPUT INFORMATION ON QUERIED POINT, UNLESS REPEATING ......
            WRITE (*,96862)  IVALU(1),RVALU(2)
96862       FORMAT (' VALUE    ',I11,' FREQUENCY ',F11.1)
            GO TO 68900
C
C......PROCESS TYPE 7 MAP ......
68700  CONTINUE
C...........READ CELL VALUE FROM OLDMAP ......
            CALL DSIN (ICHCL,ITEMP(1),CELL,IER)
            IF (IER .NE. 0)  GO TO 85040
            IF (ITEMP(1) .GT. 0)  GO TO 68720
C................OUTPUT INFORMATION ON BACKGROUND POINT, UNLESS REPEATING ...
                 WRITE (*,96872)
96872            FORMAT (' VALUE = BACKGROUND')
                 GO TO 68900
68720       CONTINUE
C...........READ VALUE-COUNT FOR CELL ......
            CALL DSIN (ICVC,IVALU(1),FLOAT(ITEMP(1)),IER)
            IF (IER .NE. 0)  GO TO 85040
            IF (NWORD .EQ. 0)  GO TO 68740
C................READ ATTRIBUTE-DESCRIPTOR ......
                 CALL DSIN (ICAD,JBUFF(1),FLOAT(ITEMP(1)),IER)
                 IF (IER .NE. 0)  GO TO 85040
68740       CONTINUE
C................OUTPUT INFORMATION ON QUERIED POINT ......
                 WRITE (*,96874)  RVALU(1),RVALU(2)
96874            FORMAT (' VALUE  ',F13.4,' FREQUENCY ',F11.1)
                 IF (NWORD .LE. 0) GO TO 68900
                      WRITE (*,96876) (JBUFF(I),I=1,NWORD)
96876                 FORMAT (' LABEL   ',32A2)
                 GO TO 68900
C
C......PROCESS TYPE 8 MAP ......
68800  CONTINUE
C...........READ CELL VALUE FROM OLDMAP ......
            CALL DSIN (ICHCL,ITEMP(1),CELL,IER)
            IF (IER .NE. 0)  GO TO 85040
            RVALU(1) = FLOAT(ITEMP(1))
            IF (LEN .GT. 1)  RVALU(1) = RTEMP
C...........OUTPUT INFORMATION ON QUERIED POINT ......
            WRITE (*,96818)  RVALU(1)
96818       FORMAT (' VALUE  ',F13.4)
            ifound=1
C
68900  CONTINUE
       XX = 0.
       YY = 0.
       GO TO 90000
C
C......ERROR RETURNS .....
C
85020  CONTINUE
       WRITE (*,98502) (MAP(I),I=1,10),IER
98502  FORMAT(' *QUERCL* ERROR OPENING ',10A2,'  ERROR',I5)
       GO TO 90000
C
85040  CONTINUE
       WRITE (*,98504) IER
98504  FORMAT(' *QUERCL* ERROR FROM DSIN  ERROR',I5)
       GO TO 90000
C
85060  CONTINUE
       WRITE(*,98506)
98506  FORMAT(' *QUERCL* CURSOR OUT OF WINDOW OR MULTIPLE KEY STRIKE')
C
C......JUMP OUT .....
90000  CONTINUE
C
C.....CLOSE UP CHANNELS .....
       IF (NWORD .GT. 0) CALL DSCLS(ICAD,IER)
       IF (ICTYP .EQ. 7) CALL DSCLS(ICVC,IER)
       CALL DSCLS(ICHCL,IER)
C
       RETURN
       END
