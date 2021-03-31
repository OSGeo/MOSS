      SUBROUTINE KRGIN2 (IOUT,MAPID,IDTYPE,IATYP,LOCATE,
     1   WIND,SCALEZ,AWIND,ICHAN1,ICHAT,ICHSTP,
     2   NAME, NPTS,IER)
C************************************************************************
C
C     NAME:  SUBROUTINE KRIGIN2
C
C     FUNCTION:  THIS ROUTINE BUILDS AN UNFORMATTED BINARY FILE
C     OF X,Y, AND ELEVATION DATA FOR USE IN THE STATPAC KRIGING
C     ROUTINES WRITTEN BY W.D.GRUNDY.  FILE IS GIVEN THE
C     NAME 'TEMPKRIG.STP',AND IS DELETED WHEN KRIGING IS COMPLETE
C
C     PARAMETERS:  NAME     USE     TYPE     DIMENSION     RANGE
C                  -------------------------------------------------
C                  IOUT     I      I*2                    +INTEGER
C                  THE OUTPUT CONSOLE CHANNEL FOR MESSAGES
C                  -------------------------------------------------
C                  MAPID    I      I*2                    +INTEGER
C                  ACTIVE MAP TABLE NUMBER
C                  -------------------------------------------------
C                  IDTYPE   I      I*2                    +INTEGER
C                  VECTOR MAP TYPE 1,5,11,12,13
C                  -------------------------------------------------
C                  IATYP     I      I*2                     +INTEGER
C                  LENGTH OF MULTIPLE ATTRIBUTE VALUE
C                  LENGTH OF ZERO INDICATION NO MULTIPLE ATTRIBUTE USED
C                  -------------------------------------------------
C                  LOCATE    I      I*2                    +INTEGER
C                  RECORD ADDRESS OF ATTRIBUTE HEADER RECORD ON ICHAT
C                  --------------------------------------------------
C                  WIND      I      R*4           4         REAL
C                  CURRENT VIEWING WINDOW--XMIN,XMAX,YMIN,YMAX
C                  -------------------------------------------------
C                  SCALEZ    I      R*4                     REAL
C                  SCALE FACTOR TO MULTIPLE ELEVATIONS BY
C                  -------------------------------------------------
C                  AWIND     I      R*4           4         REAL
C                  NEW MAPS WINDOW--XMIN,XMAX,YMIN,YMAX
C                  --------------------------------------------------
C                  XSIDE     I      R*4                     +REAL
C                  WIDTH OF CELL
C                  -------------------------------------------------
C                  YSIDE     I      R*4                     +REAL
C                  HEIGHT OF CELL
C                  -------------------------------------------------
C                  ICHAN1    I      I*2                     +INTEGER
C                  CHANNEL TO THE VECTOR MAP
C                  -------------------------------------------------
C                  ICHAT     I      I*2                     +INTEGER
C                  CHANNEL TO THE MULTIPLE ATTRIBUTE FILE
C                  -------------------------------------------------
C                  ICHSTP    I      I*2                     +INTEGER
C                  CHANNEL TO THE STATPAC DATA FILE
C                  -------------------------------------------------
C                  NAME      I      I*2            16       CHARACTER
C                  USER-SPECIFIED NAME FOR OUTPUT CELL FILE
C                  -------------------------------------------------
C                  NPTS      O      I*2                    +INTEGER
C                  NUMBER OF ROW,COLUMN AND X,Y,ELEVATION VALUES
C                  -------------------------------------------------
C                  IER       O      I*2                     INTEGER
C                  ERROR INDICATOR 1-NO ERROR
C                  -------------------------------------------------
C
C     LAYOUT OF DATA FILE CREATED BY MOSS FOR INPUT TO STATPAC PROGRAMS
C
C     HEADER RECORD:
C       WORD #   TYPE         DESCRIPTION
C       ------   ----         -----------
C        1-4     PACKED CHAR  NAME OF DATA-UNUSED IN THIS APPLICATION AND
C                             SET TO BLANK.
C          5     INT*2        NO OF DATA RECORDS
C          6     INT*2        NO OF VALUES FOR EACH DATA POINT--SET TO
C                             A CONSTANT '3' FOR THIS APPLICATION.
C       7-10     PACKED CHAR  NAME OF FIRST COLUMN OF VALUES--SET TO
C                             CONSTANT 'EAST' FOR THIS APPLICATION
C      11-14     PACKED CHAR  NAME OF SECOND COLUMN OF VALUES--SET TO
C                             CONSTANT 'NORTH' FOR THIS APPLICATION
C      15-18     PACKED CHAR  NAME OF THIRD COLUMN OF VALUES--SET TO
C                             CONSTANT 'ASSAYVAL' FOR THIS APPLICATION
C
C     DATA RECORDS:
C       WORD #   TYPE         DESCRIPTION
C       ------   ----         -----------
C          1     INT          RECORD NUMBER
C        2-9     PACKED CHAR  RECORD IDENTIFIER--NOT USED AND SET TO
C                             BLANKS FOR THIS APPLICATION
C      10-13     INT*4,DI-    LAT-LON LOCATION OF DATA POINT--NOT USED
C                MENSION 2    AND SET TO ZEROES FOR THIS APPLICATION
C      14-15     REAL*4       FIRST VALUE (X COORDINATE)
C      16-17     REAL*4       SECOND VALUE (Y COORDINATE)
C      18-19     REAL*4       THIRD VALUE (ELEVATION, ASSAY VALUE, OR
C                             OTHER DATA BEING INTERPOLATED)
C         20     BINARY       PACKED QUALIFYING CODES FOR THE VALUES,
C                             THREE BITS PER VALUE--NOT USED AND SET TO
C                             ZERO FOR THIS APPLICATION.
C
C     ROUTINES REQUIRED: ADDSTP,Qapget,FNUM,GETAT,GETFAST,SCAGET,UNPACK
C
C***********************************************************************
C
C
       common /gridpts/ xmin,xside,ymax,yside
       INTEGER IBUFF(128),ISUB(128),IRC(2),IXYZ(8),itemp(8),NPTS,INUM,
     +         IZER
       INTEGER*4 I4V,LOC(2)
       REAL WIND(4),FWIND(4),AWIND(4),XYZ(4)
       DIMENSION NAME(16)
       CHARACTER*8 BLNK,NORTH,EAST,ASSAY
       EQUIVALENCE (IXYZ(1),XYZ(1))
       DATA BLNK/8H         /,NORTH/'NORTH   '/,EAST/'EAST    '/,
     +      ASSAY/'ASSAYVAL'/
C
      ILEN1  = 256
      LOC(1) = 0
      LOC(2) = 0
      IZER   = 0
      ier=0 
C
C......OPEN CONSOLE FOR CONTINUOUS UPDATE
C
C......GET BOUNDS ON MAPID POINTER SET .....
C
       CALL apget(MAPID,ISTART,ISTOP,NPTS)
C
C......SET NUMBER OF POINTS .....
C
       NPTS=0
C
C.....OPEN CHANNEL TO STATPAC FILE
C
C      WRITE (NPRNT,1003)
 1003 FORMAT (8X,'TRANSLATE PC-MOSS FILE TO STATPAC FILE')
      ITEMP(1)=2HTE
      ITEMP(2)=2HMP
      ITEMP(3)=2HKR
      ITEMP(4)=2HIG
      ITEMP(5)=2H.S
      ITEMP(6)=2HTP
      ITEMP(7)=0
      CALL openf(ICHSTP,iteMP,3,60,2,IER)
      IF(IER .NE. 0) GO TO 85080
C
C.....WRITE STATPAC HEADER
C
      INUM=3
      WRITE (ICHSTP) BLNK,NPTS,INUM,EAST,NORTH,ASSAY
C
C......NOW GET THE ELEVATION DATA .....
C
       DO 2000 I=ISTART,ISTOP
C
         CALL GETFAST(I,FWIND,INDEX,ISUBP,IEX1,IEX2,IER)
         IF(IER.NE.0) GO TO 80000
C
C........IS THIS FEATURE IN THE WINDOW ? .....
C
         IF (FWIND(1) .GT. WIND(2)  .OR.  FWIND(2) .LT. WIND(1)  .OR.
     +       FWIND(3) .GT. WIND(4)  .OR.  FWIND(4) .LT. WIND(3))
     +       GO TO 2000
C
C........GET FEATURE RECORD .....
C
         CALL rdblk(ICHAN1,index,IBUFF,1,IER)
         IF (IER .NE. 0) GO TO 85040
C
C........GET (X,Y) VALUE OF THIS LOCATION FOR TYPE 1 AND 5 .....
C
         XYZ(1) = FWIND(1)
         XYZ(2) = FWIND(3)
C
C........GET ELEVATION FOR TYPE 5 .....
C
         IF (IDTYPE .NE. 5) GO TO 1100
           XYZ(3)=IBUFF(10)
           GO TO 1800
C
C........GET ELEVATION FOR TYPE 11,12,13 .....
C
 1100    CONTINUE
         IF (IDTYPE .LT. 11) GO TO 1600
           CALL SCAGET(IBUFF,SCALE)
           INDEX = INDEX + ((IBUFF(26)-99)/128)+1
           CALL rdblk(ICHAN1,index,IBUFF,1,IER)
           IF (IER .NE. 0) GO TO 85040
           XYZ(1) = (FLOAT(IBUFF(1))*SCALE) + FWIND(1)
           XYZ(2) = (FLOAT(IBUFF(2))*SCALE) + FWIND(3)
           XYZ(3) =  FLOAT(IBUFF(3))
           GO TO 1800
C
C........GET ELEVATION FOR TYPE 1 .....
C
 1600    CONTINUE
         IF (IDTYPE.NE.1) GO TO 80020
C........ELEVATION IN SUBJECT .....
         IF (IATYP.GT.0) GO TO 1640
           CALL rdblk(ICHAN1,isubp,ISUB,1,IER)
           IF (IER .NE. 0) GO TO 85040
           GO TO 1660
C........ELEVATION IN MULTIPLE ATTRIBUTE FILE .....
 1640    CONTINUE
         ITEMN = IBUFF(5)
           CALL GETAT(ICHAT,IATYP,IATYP,LOCATE,IBUFF,ITEMN,ISUB,RNUM,
     +                IER)
           XYZ(3) = RNUM
           IF (IATYP .EQ. 1  .OR.  IATYP .EQ. 2) GO TO 1800
C
C........FIND FIRST NONBLANK CHARACTER IN CHARACTER STRING .....
C
 1660    CONTINUE
           CALL UNPACK(ISUB,80)
           DO 1680 K=1,10
             IF (ISUB(K).GT.32) GO TO 1690
 1680      CONTINUE
 1690      CONTINUE
           K = K - 1
           CALL FNUM(ISUB,80,XYZ(3),K)
C
C***.....WRITE STATPAC DATA RECORD AND UPDATE SCREEN DISPLAY
C
 1800      CONTINUE
           XYZ(3)=  XYZ(3) * SCALEZ
           NPTS =  NPTS + 1
C           WRITE (ICHSTP) NPTS,BLNK,BLNK,LOC(1),LOC(2),
C     1         (XYZ(J),J=1,3),IZER
           WRITE (ICHSTP) NPTS,BLNK,BLNK,LOC(1),LOC(2),
     1         XYZ(1),XYZ(2),XYZ(3),IZER
C           IF (NPTS/10*10.EQ.NPTS) WRITE (NPRNT,1900) NPTS
 1900      FORMAT ('+',I5)
C
C.....LOOP BACK FOR NEXT DATA POINT
C
 2000  CONTINUE
C      WRITE (NPRNT,2010) NPTS
 2010  FORMAT ('+',I5)
C
C......REWRITE HEADER RECORD AND CLOSE FILE
C
        read(ichstp,rec=1)blnk,npt,inum,east,north,assay
        write(ichstp,rec=1)blnk,npts,inum,east,north,assay
C
C        CALL HCHEAT( NPTS )
C
C......NORMAL RETURN .....
C
       GO TO 90000
C
C......ERROR RETURNS .....
C
80000  CONTINUE
       WRITE(IOUT,98000) IER
98000  FORMAT(' *KRIGIN2*  ERROR FROM GETFAST  ERROR',I5)
       GO TO 90000
C
80020  CONTINUE
       IER = 2
       WRITE(IOUT,98002) IDTYPE,IER
98002  FORMAT(' *KRIGIN2*  ILLEGAL DATA TYPE TYPE ',I5,'  ERROR',I5)
       GO TO 90000
C
85040  CONTINUE
       WRITE(IOUT,98504) IER
98504  FORMAT(' *KRIGIN2*  ERROR FROM DSIN  ERROR',I5)
       GO TO 90000
C
85060  CONTINUE
       WRITE(IOUT,98506) IER
98506  FORMAT(' *KRIGIN2*  ERROR FROM DSOUT  ERROR',I5)
       GO TO 90000
C
85080  CONTINUE
       WRITE(IOUT,98508) IER
98508  FORMAT(' *KRGIN2*  ERROR FROM OPEN OF TEMPKRIG.STP  ERROR',I5)
C
C......JUMP OUT .....
C
90000  CONTINUE
       RETURN
       END
