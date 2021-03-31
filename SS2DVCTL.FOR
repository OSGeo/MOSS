      SUBROUTINE SS2DVCTL(ko,ier)
C***
C***  THIS PROGRAM CREATES A CONTROL FILE FOR USE BY PROGRAM SS2DGAMH.
C***
C***  REFERENCE: SEE PROGRAM UGAMM IN
C***  Knudsen, H.P., and Kim, Y.C., 1977, A short course on geostatist-
C***      ical ore reserve estimation: College of mines, University of
C***      Arizona, Tucson, Arizona, 202 pages and appendices.
C***
C***                   DESCRIPTION OF CONTROL FILE CREATED
C***
C*** RECORD COL(S) FORMAT  VARIABLE          PURPOSE AND COMMENTS
C***   NO.  USED             NAME
C***
C***   1    1-79   19A4,A3   HEAD    IDENTIFICATION FOR VARIOGRAM RUN.
C***   2     1      I1       LOG     OPTION FOR NATURAL LOG TRANSFORM-
C***                                 ATION OF DATA; 1=YES;0=NO. USUALLY 0.
C***   2     3      I1       IPC     PUNCHOUT OPTION FOR PROGRAM VARPLOT.
C***                                 1=YES;0=NO.
C***   2     5      I1       LL      VARIOGRAM DISPLAY OPTION. 1=MOMENT
C***                                 CENTER;0=GAMMA(H). USUALLY 0.
C***                                 FOR EXPLANATION OF MOMENT CENTER
C***                                 SEE KNUDSEN AND KIM, P119-120.
C***   2     7      I1       ISCAL   AUTOMATIC SCALING OF GRAPH. 1=YES;
C***                                 0=NO. USUALLY 1.
C***   2     9      I1       NDIR    NUMBER OF VARIOGRAM DIRECTIONS SPECI-
C***                                 FIED. MAXIMUM ALLOWED IS FIVE (5).
C***   2   13-24    F12.3    CLAS    CLASS INTERVAL TO BE USED TO GROUP
C***                                 DISTANCES. IT IS ALSO USED TO LIMIT
C***                                 THE DISTANCE BETWEEN HOLES FOR WHICH
C***                                 THE VARIOGRAM IS TO BE COMPUTED, I.E.,
C***                                 20*CLASS=MAXIMUM DISTANCE(H).
C***   2   24-40    F15.5    TMAX    MAXIMUM VARIOGRAM VALUE ON Y-AXIS OF GRAPH
C***                                 WHEN AUTOMATIC SCALING OPTION IS NOT USED.
C***                                 IF ISCAL=1, THIS VALUE IS NOT USED.
C***   3-1  1-6     F6.2     DANG(1) ANGLE IN DEGREES ALONG WHICH THE VARIO-
C***                                 GRAM IS TO BE CALCULATED:
C***                                 0 = EAST-WEST     45 = NORTHEAST-SOUTHWEST
C***                                 90= NORTH-SOUTH   -45= NORTHWEST-SOUTHEAST
C***   3-1 12-16    F6.2     DSPR(1) ALLOWABLE DEVIATION IN DEGREES.
C***                                 ADD ONLY AS MANY ADDITIONAL ANGLE AND DEV-
C***                                 IATION RECORDS AS NECESSARY TO EQUAL THE
C***                                 NUMBER OF DIRECTIONS (NDIR) SELECTED.
C***   3-2  1-6     F6.2     DANG(2) ADD IF NDIR.GE.2
C***   3-2 12-16    F6.2     DSPR(2) ADD IF NDIR.GE.2
C***   3-3  1-6     F6.2     DANG(3) ADD IF NDIR.GE.3
C***   3-3 12-16    F6.2     DSPR(3) ADD IF NDIR.GE.3
C***   3-4  1-6     F6.2     DANG(4) ADD IF NDIR.GE.4
C***   3-4 12-16    F6.2     DSPR(4) ADD IF NDIR.GE.4
C***   3-5  1-6     F6.2     DANG(5) ADD IF NDIR.EQ.5
C***   3-5 12-16    F6.2     DSPR(5) ADD IF NDIR.EQ.5
C***   4     1      I1       ISEL    DATA SELECTION OPTION. 1=YES;0=NO.
C***                                 IF YES, THE BOUNDARIES WITHIN WHICH
C***                                 TO SEARCH THE ASSAY DATA MUST BE
C***                                 SPECIFIED ON THIS RECORD AS FOLLOWS:
C***   4    3-14    F12.3    YMIN    MINIMUM NORTH COORDINATE IF ISEL=1
C***                                 OTHER WISE NOT USED.
C***   4   16-27    F12.3    YMAX    MAXIMUM NORTH COORDINATE IF ISEL=1
C***   4   29-40    F12.3    XMIN    MINIMUM EAST-COORDINATE IF ISEL=1
C***   4   42-53    F12.3    XMAX    MAXIMUM EAST COORDINATE IF ISEL=1
C***
C***         DESCRIPTION OF PROCESS COMPLETION CODES
C***   ICHERR  Channel number to write Interprocess Communication
C***   IER     Error code returned from this process
C***           0 means normal completion; -1 means abnormal
C***           See comments in program PROCES.FR
C***
C***  LINKS REQUIRED: CHEKER,CKSAFE,CONCAT,SOUND
C***  FILE NAME REQUIREMENTS
C***       THE CHARACTER OUTPUT FILE IS AUTOMATICALLY GIVEN THE
C***       EXTENSION .FVC (Formatted VCtl).
C*** THE FOLLOWING STATEMENT IS INCLUDED TO REDUCE PROGRAM SIZE AND
C*** DECREASE EXECUTION TIME. GEORGE HEINE, BLM DENVER, 12/5/85.
      LOGICAL FLAG
      CHARACTER*1 IANS
      CHARACTER*4 EXT,ID(20),idvst
      CHARACTER*32 UNIT21
      common /dvctl/ DANG(5),DSPR(5),logg,iipc,Ill,iscali,inrel,indir,
     + cclas,ttmax,iisel,yymin,yymax,xxmin,xxmax
      common /CHAR/id,idvst(4)
      DATA EXT/'.FVC'/
      DATA IOK,IBAD /0,-1/
      KO=40
      IOUT=6
C
      UNIT21='TEMPVARG.FVC'
      iscali=1
C      OPEN(KO,file=unit21,STATUS='UNKNOWN',FORM='UNFORMATTED')
      WRITE(IOUT,10)
   10 FORMAT(/,' ENTER 79 CHARACTER TITLE FOR VARIOGRAM : ',/,' ? '\)
      READ(*,20)ID
   20 FORMAT(19A4,A3)
      WRITE(IOUT,30)
   30 FORMAT(' LOGARITHMIC CONVERSION WANTED? [N]:',/,' ? '\)
      READ(*,40)IANS
   40 FORMAT(A1)
      IF(IANS.EQ.'Y'.OR.IANS.EQ.'y') LOGG=1
      WRITE(IOUT,50)
   50 FORMAT(' IS MOMENT CENTER DISPLAY OPTION WANTED? [N]:',/,' ? '\)
      READ(*,40)IANS
      IF(IANS.EQ.'Y'.OR.IANS.EQ.'y') ILL = 1
      WRITE(IOUT,60)
   60 FORMAT(' IS RELATIVE VARIOGRAM WANTED? [N]:',/,' ? '\)
      READ(*,40)IANS
      IF(IANS.EQ.'Y'.OR.IANS.EQ.'y') INREL=1
      WRITE(IOUT,70)
   70 FORMAT(' IS AUTOMATIC SCALING OF THE GRAPH WANTED? [Y]:',/,' ? '\)
      READ(*,40)IANS
      IF(IANS.EQ.'N'.OR.IANS.EQ.'n') ISCALI=0
      IF(ISCALI.EQ.1) GO TO 90
   75 WRITE(IOUT,80)
   80 FORMAT(' GIVE MAXIMUM VARIOGRAM VALUE FOR SCALING.',/,' ? '\)
      READ(*,130,IOSTAT=IER)TTMAX
      IF ((IER.NE.0).OR.(TTMAX.LE.1E-15)) THEN
        write(*,*)char(bel)
        WRITE (IOUT,90010)
        GO TO 75
        ENDIF
   90 CCLAS = 0.0
      WRITE(IOUT,100)
  100 FORMAT(' GIVE CLASS INTERVAL (IN METERS) TO GROUP DISTANCES',
     1     /,' [DEFAULT = 1000]:',/,' ? '\)
      READ(*,130,IOSTAT=IER)CCLAS
      IF (CCLAS.EQ.0.0) CCLAS = 1000.0
      IF ((IER.NE.0).OR.(CCLAS.LE.1E-15)) THEN
        write(*,*)char(bel)
        WRITE (IOUT,90010)
        GO TO 90
        ENDIF
  110 WRITE(IOUT,120)
  120 FORMAT(' NUMBER OF VARIOGRAM DIRECTIONS? MAX = 5 :',/,' ? '\)
      READ(*,140,IOSTAT=IER)INDIR
      IF ((INDIR.LE.0).OR.(INDIR.GT.5).OR.(IER.NE.0)) THEN
        write(*,*)char(bel)
        WRITE (IOUT,90020)
        GO TO 110
        ENDIF
  130 FORMAT(BN,F10.0)
  140 FORMAT(BN,I1)
  160 DO 190 I=1,INDIR
  165 WRITE(IOUT,170)I
  170 FORMAT(' ENTER ANGLE (Dec. Deg(azimuth)) FOR VARIOGRAM ',
     + I1,/,' ? '\)
      READ(*,220,IOSTAT=IER)DANG(I)
      IF (IER.NE.0) THEN
        write(*,*)char(bel)
        WRITE (IOUT,90030)
        GO TO 165
        ENDIF
C
C  CONVERT ENTERED ANGLE FROM NORTH=0 TO EAST=0 SYSTEM
C
      DANG(I) = 450 - DANG(I)
  175 WRITE(IOUT,180)I
  180 FORMAT(' ENTER WINDOW (DEC DEG) FOR VARIOGRAM ',I1,/,' ? '\)
      READ(*,220,IOSTAT=IER)DSPR(I)
      IF (IER.NE.0) THEN
        write(*,*)char(bel)
        WRITE (IOUT,90030)
        GO TO 175
        ENDIF
      WRITE (*,*)
  190 CONTINUE
  220 FORMAT (BN,F10.0)
c  260 WRITE(KO)ID
c      WRITE(KO) LOG,IPC,LL,ISCAL,NREL,NDIR,CLAS,TMAX
c  270 FORMAT(6(I1,1X),F12.3,1X,F15.5)
c      WRITE(KO) (DANG(I),DSPR(I),I=1,NDIR)
c  280 FORMAT(F6.2,5X,F6.2)
c      WRITE(KO)ISEL,YMIN,YMAX,XMIN,XMAX
c  290 FORMAT(I1,1X,4(F12.3,1X))
c      close(ko)
C
C     WRITE 'NORMAL COMPLETION' CODE TO IPC FILE
C
C*** ERROR MESSAGES
C
90010 FORMAT (' You must enter a positive real number. Try again.')
90020 FORMAT (' You must enter an integer between 1 and 5. Try again.')
90030 FORMAT (' You must enter a real number. Try again.')
      return
C***
      END
