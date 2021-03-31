        SUBROUTINE KRIG(NPRNT,IOIN,ICHAN1,ICHAN5,ICHAT,
     +    ICHKPR,ICHM,ICHOUT,ICHSTP,ICHV1,ICHV2,WIND,MAPID,IDTYPE,IATYP,
     +    LOCATE,MTYP,MASKM,IBW,LEN,LABEL,IHEAD,SCALEZ,
     +    AWIND,DEFLT,MAXR,MAXC,YMIN,IER)
C
C*********************************************************************
C    S U B R O U T I N E   K R I G
C
C    FUNCTION: Serves as link between MOSS and the STATPAC Kriging
C              routines.
C
C    DUMMY ARGUMENTS:
C      Data Channels    NPRNT  int    Console output
C                       IOIN   int    Console input
C                       ICHAN1 int    Input map of point data
C                       ICHAN2 int    Output to .DH file
C                       ICHAN5 int    Output cell map header
C                       ICHAT  int    Input multiple attribute file
C                       ICHERR int    I/O file for error messages from
C                                       swapped processes
C                       ICHKPR int    File for passing user parameters
C                                       to swapped processes
C                       ICHM   int    Input masking map file
C                       ICHOUT int    Output cell file
C                       ICHSTP int    Statpac-format file passed
C                                       to swapped processes
C                       ICHV1 int     Output cell header file for
C                                       map of variances
C                       ICHV2 int     Output cell file for map of variances
C
C      Input Map        WIND rl arr   Current viewing window
C                       MAPID  int    Position of input map in fast file
C                       IDTYPE int    ID type of input map
C                       IATYP  int    Length of attribute in words:
C                                     0=no att.;1=integer;2=real;3+=char
C                       LOCATE int    Address of attribute in .AT file
C                       MWORDS int
C
C      Masking Map      MTYP  int     Data type of masking map
C                       MASKM
C                       IBW
C                       LEN
C
C      Output Cell      LABEL int arr Name of cell map
C        Map(s)         IHEAD int arr Header record
C                       IWORK int arr Name of project ('POLYGON')
C                       SCALEZ rl     Scaler to multiply z-values
C                       AWIND rl arr  MBR of cell map
C                       DEFLT rl      value to assign masked cells
C                       MAXR int      number of rows
C                       MAXC int      number of columns
C                       XSIDE rl      e-w dimension of cells
C                       YSIDE rl      n-s dimension of cells
C                       XMIN rl       easting of SW corner
C                       YMIN rl       northing of SW corner
C                       YMAX rl       northing of NW corner
C
C *****************************************************************
$include:'dnames.inc'
       COMMON /STRNG/ ICARD(80),ICARDP,LENICD
       common /gridpts/ xmin,xside,ymax,yside
C
C
       external  DSIN
       LOGICAL VARMAP,iexist
       CHARACTER*1 IANS
       character*25 ftempr
       INTEGER*2 HEADER(22),IHOLID(8),LOC(4),IMASK(2),MAP(15)
       INTEGER*2 LABEL(16),LABELV(16),IHEAD(256),FTEMP(10)
       INTEGER*2 MIN(2),MAX(2),IVAR(2),IVAL(2),PARENT(16),CHILD(16)
       REAL MASK,WIND(4),AWIND(4)
C
       EQUIVALENCE (IMASK(1),MASK),(IVAL(1),VAL),(IVAR(1),VAR)
       EQUIVALENCE (MIN(1),ZMIN),(MAX(1),ZMAX)
       equivalence (ftemp(1),ftempr)
       DATA BIG/.9999E38/
       DTMP = DEFLT
C   SET UP PARENT NAME
C
       IF(IBW .EQ. 2) THEN
           ILEN = LEN/8
        ELSE
           ILEN = LEN*2
       ENDIF
       ILENP = ILEN
C
       WRITE (NPRNT,10)
   10  FORMAT (//,15X,'*** WELCOME TO KRIGING ***'/)
C
C   Write grid parameters to parameter file
C
       CALL KRGIN1 (NPRNT,ICHKPR,LABEL,DSNAM,MAXR,MAXC,
     1                YMIN)
C
C   Ask user if this is a restart; if so, check that file exists
C
c       WRITE (NPRNT,30)
c   30  FORMAT (' Is this a restart?  [N]',/,' ?'\)
c       READ (IOIN,35) IANS
c   35  FORMAT (A1)
c       IF ((IANS.NE.'y').AND.(IANS.NE.'Y')) GO TO 50
       go to 50
       ichupr=18
       FTEMP(1)=2HTE
       FTEMP(2)=2HMP
       FTEMP(3)=2HKR
       FTEMP(4)=2HIG
       FTEMP(5)=2H.U
       FTEMP(6)=2HPR
       FTEMP(7)=0
       call openf(ichupr,ftemp,1,0,1,ier)
       IF (ier.eq.0) GO TO 90
       WRITE (NPRNT,40)
   40  FORMAT (' Previous STATPAC file not found...restart option ',
     +         'ignored')
C
C   Not a restart; Get elevation data from MOSS file and write to STATPAC file
C
   50  CALL KRGIN2 (NPRNT,MAPID,IDTYPE,IATYP,LOCATE,WIND,SCALEZ,
     1               AWIND,ICHAN1,ICHAT,ICHSTP,DSNAM,
     2               NPTS,IER)
C
C   Sort STATPAC file in preparation for Kriging
C
        call ss2dprep(ichstp,ichupr,ier)
       IF (IER.NE.0) GOTO 90200
C
C   Ask user if he wants a map of Kriging variances;
C   if so, get map name from keyboard
C
   90  CONTINUE
       CALL DSCLS (ICHAN1,IER)
       VARMAP = .FALSE.
       WRITE (NPRNT,100)
  100  FORMAT (/,' DO YOU WANT A MAP OF KRIGING VARIANCES? '\)
       READ (IOIN,110) IANS
  110  FORMAT (A1)
C      IANS = IANS/256
       IF ((IANS.NE.'Y').AND.(IANS.NE.'y')) GO TO 300
         VARMAP = .TRUE.
  120    CALL whtcal (LABELV,IIER)
         CALL mchek(LABELV,IFIL,I,ITYP,IER)
         IF (IFIL.NE.0) WRITE (NPRNT,130)
  130     FORMAT(' THIS MAP ALREADY EXISTS.  PLEASE TRY AGAIN')
         IF (IFIL.NE.0) GO TO 120
C
C   Get variogram parameters from keyboard and
C   calculate Kriging values and variances
C
  300  CONTINUE
C
        call ss2dgrid(ichupr,ichugr,ier)
       IF (IER.NE.0) GO TO 90300
C
C   Open channel to masking map, if any
C
       IF (ICHM .EQ. 0) GO TO 500
       CALL gtnam(-1,MTYP,MASKM,MAP,IER)
       CALL DSOPN(ICHM,MAP,1,1,IBW,LEN,128,IER)
       IF(IER.NE.0) GO TO 90400
C
C   Create and open output cell & header files
C
 500   CONTINUE
       CALL gtnam(-1,8,LABEL,MAP,IER)
       CALL DSOPN(ICHOUT,MAP,3,3,1,2,128,IER)
       IF(IER.NE.0) GO TO 90500
C
       CALL gtnam(-1,5,LABEL,MAP,IER)
       CALL DSOPN(ICHAN5,MAP,3,3,1,256,256,IER)
       IF(IER.NE.0) GO TO 90500
C
C   If variance map is wanted, open
C   cell and header files
C
       IF (.NOT.VARMAP) GO TO 1000
         CALL gtnam (-1,5,LABELV,MAP,IER)
         CALL DSOPN (ICHV1,MAP,3,3,1,256,256,IER)
            IF (IER.NE.0) GO TO 90600
         CALL gtnam(-1,8,LABELV,MAP,IER)
         CALL DSOPN (ICHV2,MAP,3,3,1,2,128,IER)
            IF (IER.NE.0) GO TO 90600
C
C   Open channel to STATPAC '.UGR' file containing
C   the Kriged data
C
 1000  CONTINUE
c      FTEMP(1) = 2HTE
c      FTEMP(2) = 2HMP
c      FTEMP(3) = 2HKR
c      FTEMP(4) = 2HIG
c      FTEMP(5) = 2H.U
c      FTEMP(6) = 2HGR
c      FTEMP(7) =  0
c       CALL openf(ICHSTP,FTEMP(1),1,4096,1,IER)
c       IF(IER .NE. 0) GO TO 90700
c       READ(ICHSTP,IOSTAT=IER,ERR=90900) (HEADER(J),J=1,22)
C       READ(ICHSTP,IOSTAT=IER,ERR=90900) (HEADER(J),J=1,44)
C       READ(ICHSTP,IOSTAT=IER,ERR=90900) I4D(1),I4D(2),NII,MII,
C     .   (I4D(j) , I4D(j) , j = 1,4)
       rewind(ichugr)
C       WRITE (NPRNT,1003)
 1003  FORMAT (3X, ' TRANSLATING STATPAC FILE BACK TO MOSS ')
C
C   Loop to read the Kriged data and write it to the cell file(s)
C
       ZMAX  = -BIG
       ZVMAX = -BIG
       ZMIN  =  BIG
       ZVMIN =  BIG
       DO 4000 IROW=1,MAXR
         YCELL=YMIN + (FLOAT(IROW-1)*YSIDE) + (YSIDE/2.0)
         DO 2000 ICOL=1,MAXC
           CELL = ((IROW-1) * MAXC) + ICOL
           XCELL=XMIN + (FLOAT(ICOL-1)*XSIDE) + (XSIDE/2.0)
C           VAL = DEFLT
C
C..........IS THIS CELL MASKED  IF SO THEN SKIP IT .....
           IF (ICHM .EQ. 0)  GO TO 1200
C  ilenp used for byte count     
             CALL DSIN(ICHM,IMASK,CELL,IER)
             IF (IER .NE. 0) GO TO 90800
             IF (MASK .NE. 0) GO TO 1800
 1200      CONTINUE
C
C..........Read a Kriged record from .UGR file............
C
C           READ BINARY (ICHSTP,ERR=90900) IREC,(IHOLID(I),I=1,8),(LOC(I),I=1,4),
C     1      XCOORD,YCOORD,VAL,VAR,NULL
C       write(*,*) 'krig: reading ugr data'
           READ (ICHUGR,IOSTAT=IER,ERR=90900) IREC,
     1          XCOORD,YCOORD,VAL,VAR,NULL
C       write(*,*) 'krig: ugr data'
C
C..........Write it to cell file...............
C
 1800      CONTINUE
           ZMIN = AMIN1(VAL,ZMIN)
           ZMAX = AMAX1(VAL,ZMAX)
           LENW=4
           CALL DSOUT(ICHOUT,IVAL,CELL,IER)
           IF  (IER .NE. 0) GO TO 91000
C..........Write out the variance if necessary
           IF ( .NOT. VARMAP) GO TO 2000
             ZVMIN = AMIN1(VAR,ZVMIN)
             ZVMAX = AMAX1(VAR,ZVMAX)
             LENW=4
             CALL DSOUT(ICHV2,IVAR,CELL,IER)
             IF  (IER .NE. 0) GO TO 91100
 2000    CONTINUE
 4000  CONTINUE
C
C   Write header now that max and minimum values are known
C
C       BYTE (IHEAD,175) = BYTE (MIN,1)
C       BYTE (IHEAD,176) = BYTE (MIN,2)
C       BYTE (IHEAD,177) = BYTE  (MIN,3)
C       BYTE (IHEAD,178) = BYTE (MIN,4)
C       BYTE (IHEAD,179) = BYTE (MAX,1)
C       BYTE (IHEAD,180) = BYTE (MAX,2)
C       BYTE (IHEAD,181) = BYTE (MAX,3)
C       BYTE (IHEAD,182) = BYTE (MAX,4)
       IHEAD(88)=MIN(1)
       IHEAD(89)=MIN(2)
       IHEAD(90)=MAX(1)
       IHEAD(91)=MAX(2)
       LENQ = 512
       recn = 1
       CALL DSOUT(ICHAN5,IHEAD,recn,IER)
       IF (IER .NE. 0) GO TO 91200
C
C   Write header of variance map if it exists
C
       IF (.NOT.VARMAP) GO TO 5000
         ZMIN = ZVMIN
         ZMAX = ZVMAX
C       BYTE (IHEAD,175) = BYTE (MIN,1)
C       BYTE (IHEAD,176) = BYTE (MIN,2)
C       BYTE (IHEAD,177) = BYTE  (MIN,3)
C       BYTE (IHEAD,178) = BYTE (MIN,4)
C       BYTE (IHEAD,179) = BYTE (MAX,1)
C       BYTE (IHEAD,180) = BYTE (MAX,2)
C       BYTE (IHEAD,181) = BYTE (MAX,3)
C       BYTE (IHEAD,182) = BYTE (MAX,4)
        IHEAD(88)=MIN(1)
        IHEAD(89)=MIN(2)
        IHEAD(90)=MAX(1)
        IHEAD(91)=MAX(2)
        LENQ = 512
        recn = 1
         CALL DSOUT (ICHV1,IHEAD(1),recn,IER)
         IF (IER .NE. 0) GO TO 91200
C
C   Close all output files
C
 5000  CALL DSCLS(ICHAN5,IER)
       CALL DSCLS(ICHOUT,IER)
       IF (VARMAP) CALL DSCLS(ICHV1,IER)
       IF (VARMAP) CALL DSCLS(ICHV2,IER)
       IF (ICHM .NE. 0) CALL DSCLS(ICHM,IER)
C
C   Update directory and write it to data base
C
c      CALL gtnam(0,1,IWORK,MAP,IER)
c      CALL openf(ICHAN2,MAP,2,512,2,IER)
c      IF(IER.NE.0) GO TO 91400
C
c      CALL gtnam(0,2,IWORK,MAP,IER)
      CALL TUPDIR(ITP1,ITP2,ITP3,LABEL,IHEAD,iwork,2,IER)
      IF (VARMAP)CALL TUPDIR(ITP1,ITP2,ITP3,LABELV,IHEAD,iwork,2,IER)
      CLOSE(2,IOSTAT=IER)
C
C  Clean up and go home
Cc
 9999 RETURN
C
C  ERROR RETURNS
C
90200 WRITE (NPRNT,90205) IER
90205 FORMAT (' ***KRIG*** ERROR ',I5,' FROM SS2DPREP')
      GO TO 9999
90300 WRITE (NPRNT,90305) IER
90305 FORMAT ('***KRIG*** ERROR ',I5,' FROM SS2DGRID')
      GO TO 9999
90400 WRITE (NPRNT,90405) IER
90405 FORMAT (' ***KRIG*** ERROR ',I5,' FROM ATTEMPT TO OPEN MASK MAP ')
      GO TO 9999
90500 WRITE (NPRNT,90505) IER
90505 FORMAT (' ***KRIG*** ERROR ',I5,' FROM ATTEMPT TO OPEN OUTPUT',
     +        'MAP ')
      GO TO 9999
90600 WRITE (NPRNT,90605) IER
90605 FORMAT (' ***KRIG*** ERROR ',I5,' FROM ATTEMPT TO OPEN VARIANCE',
     +        'MAP ')
      GO TO 9999
90700 WRITE (NPRNT,90705) IER
90705 FORMAT ('***KRIG*** ERROR ',I5,' FROM OPEN OF STATPAC .UGR FILE ')
      GO TO 9999
90800 WRITE (NPRNT,90805) IER,IROW,ICOL
90805 FORMAT (' ***KRIG*** ERROR ',I5,' FROM ATTEMPT TO READ MASK MAP',
     1       /' ROW ',I5,' COLUMN ',I5)
      GO TO 9999
90900 WRITE (NPRNT,90905) IER,IROW,ICOL
90905 FORMAT (' ***KRIG*** ERROR ',I5,' FROM ATTEMPT TO READ UGR FILE',
     1       /' ROW ',I5,' COLUMN ',I5)
      GO TO 9999
91000 WRITE (NPRNT,91005) IER,IROW,ICOL
91005 FORMAT(' ***KRIG*** ERROR ',I5,' FROM ATTEMPT TO WRITE CELL FILE',
     1      /' ROW ',I5,' COLUMN ',I5)
      GO TO 9999
91100 WRITE (NPRNT,91105) IER,IROW,ICOL
91105 FORMAT(' ***KRIG*** ERROR ',I5,' FROM ATTEMPT TO WRITE VARI FILE',
     1      /' ROW ',I5,' COLUMN ',I5)
      GO TO 9999
91200 WRITE (NPRNT,91205) IER
91205 FORMAT ('***KRIG*** ERROR ',I5,' FROM ATTEMPT TO WRITE HEADER ')
      GO TO 9999
91400 WRITE (NPRNT,91405)
91405 FORMAT (' ***KRIG*** ERROR ',I5,' FROM OPEN OF NAMES FILE ')
      GO TO 9999
      END
