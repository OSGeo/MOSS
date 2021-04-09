      SUBROUTINE CUBSPLIN(ICHAN1,ICHAN5,ICHAT, 
     1    ICHM,ICHOUT,IXFIL,MAPID,IDTYPE,IATYP, 
     2    LOCATE,MWORDS,MTYP,MASKM,IBW,LEN,LABEL,IHEAD,SCALEZ, 
     3    DEFLT,MAXR,MAXC,IER) 
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
C                       ICHPAR int    File for passing user parameters 
C                                       to swapped processes 
C                       ICHM   int    Input masking map file 
C                       ICHOUT int    Output cell file 
C                       IXFIL  int    Data files passed to swapped 
C                       IYFIL  int       processes and used as 
C                       IZFIL  int       virtual arrays 
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
C 
C      COMMON /DSBUF/ MLEN,IDATA(8448) 
C 
$include:'dnames.inc'
$include:'dbchan.inc'
      COMMON /IO/     NPRNT,IOIN 
      common/varray/ izfil,ichigp
      COMMON /STRNG/ ICARD(80),ICARDP,LENICD 
      COMMON /WINDOW/ WIND(4) 
      common/gridpts/ xmin,xside,ymax,yside
C 
      EXTERNAL DSIN 
      CHARACTER*1 IANS 
      integer*4 iwtf
      INTEGER*2 IMASK(2),IHEAD(256),MAP(25),label(16), 
     +         holdit(25),FTEMP(10),dsnam(16)
      INTEGER*2 NPRNT,IOIN,ICHAN1,ICHAN2,ICHAN5,ICHAT,ICHERR, 
     +          ICHPAR,ICHM,ICHOUT,IXFIL,IYFIL,IZFIL,MAPID,IDTYPE, 
     +     IATYP,LOCATE,MWORDS,MTYP,MASKM(10),IBW,LEN,MAXR,MAXC 
      REAL MASK,AWIND(4) 
      INTEGER IVAR(2),IVAL(2),IZMIN(2),IZMAX(2) 
      logical iexist
      EQUIVALENCE (IMASK(1),MASK),(IVAL(1),VAL),(IVAR(1),VAR), 
     1 (IZMIN(1),ZMIN), (IZMAX(1),ZMAX) 
      DATA BIG/.9999E20/ 
      DATA MAXNCP/10/ 
C 
       IWTF = 0 
C 
C......SET UP RECORD LENGTH FOR DSIN AND DSOUT...... 
       IF(IBW .EQ. 2) THEN 
           ILEN = LEN/8 
        ELSE 
           ILEN = LEN*2 
       ENDIF 
       ILENP = ILEN 
C 
      WRITE (NPRNT,10) 
   10 FORMAT (15X,'*** WELCOME TO QUINTIC SPLINE *** ',/) 
C 
C.....get user input for desired values of mode and NCP..... 
C 
  100 WRITE(NPRNT,110) 
  110 FORMAT (' ENTER NUMBER OF CLOSEST DATA POINTS TO USE', 
     1      /,' IN COMPUTATION (MAX=10): ',\) 
        call readin (ICARD,80,ICARDP,LENICD) 
        CALL INUM (ICARD,NCP,ICARDP) 
      IF ((NCP.GE.1).AND.(NCP.LE.MAXNCP)) GO TO 150 
  130 WRITE (NPRNT,140) MAXNCP 
  140 FORMAT (' VALUE MUST BE POSITIVE INTEGER, NOT GREATER THAN', 
     1      I3,/,' PLEASE TRY AGAIN. ',//) 
      GO TO 100 
  150 CONTINUE 
C      WRITE (NPRNT,160) 
  160 FORMAT ('  ......please wait a minute..........') 
C 
C     Write gridding parameters to parameter file 
C     Nulls inserted so son process can read 4-byte integers. 
C 
c       OPEN( UNIT=ICHPAR, FILE='PARAM', FORM='UNFORMATTED', 
c     +       ERR=91600, IOSTAT=IER ) 
       NUL = 0 
c       WRITE(ICHPAR) MAXR,MAXC,XMIN,YMAX,XSIDE,YSIDE 
C 
C  ASK USER IF THIS IS A RESTART;  CHECK TO SEE IF 
C  RESTART FILE EXISTS BY ATTEMPTING TO RECREATE IT 
C  NOTE:  error 25 means 'file already exists' 
C 
c        WRITE (NPRNT,162) 
c  162   FORMAT (' Is this a restart? [N] ',/,' ?'\) 
c        READ (IOIN,163) IANS 
c  163   FORMAT (A1) 
C        IANS = IANS/256 
c        IF ((IANS.NE.'Y').AND.(IANS.NE.'y')) GO TO 165 
      go to 165
      FTEMP(1)=2HTE 
      FTEMP(2)=2HMP 
      FTEMP(3)=2HKR 
      FTEMP(4)=2HIG 
      FTEMP(5)=2H.S 
      FTEMP(6)=2HTP 
      FTEMP(7)=0 
C        CALL inquire(file=FTEMP,exist=iexist) 
        IF (iexist) GO TO 200 
        WRITE (NPRNT,164) 
  164   FORMAT (' File TEMPKRIG.STP not found', 
     +          /' ......restart option ignored') 
  165   CONTINUE 
C 
C     Write files of x-coordinates, y-coordinates, and z-coordinates 
C 
C      CALL VARGIN (NPRNT,MAPID,IDTYPE,IATYP,LOCATE,MWORDS,SCALEZ, 
C     1             ICHAN1,ICHAT,IXFIL,NPTS,IER) 
       CALL KRGIN2 (NPRNT,MAPID,IDTYPE,IATYP,LOCATE,WIND,SCALEZ,
     1               AWIND,ICHAN1,ICHAT,Ixfil,DSNAM,
     2               NPTS,IER)
C
      do 166 il=1,16
166   holdit(il)=label(il)   
      call packc(label,dsnam,16)
C 
C     Write value of NPTS to parameter file now that it is known 
C     Change by GWH 7/23/86:  also write NCP to file 
C 
  200 CONTINUE 
c       WRITE(ICHPAR) NPTS,NCP 
c       CLOSE(ICHPAR) 
C 
C     Swap in Akima's algorithm to perform bivariate interpolation 
c
      call akima(maxr,maxc,npts,ncp,ixfil,ier)
c
      IF (IER.NE.0) GO TO 90100 
C 
C     Open channel to masking map, if any 
C 
       IF (ICHM .EQ. 0) GO TO 500 
       call gtnam(-1,MTYP,MASKM,MAP,IER) 
       CALL DSOPN(ICHM,MAP,1,1,IBW,LEN,1024,IER) 
       IF(IER.NE.0) GO TO 90400 
C 
C     Create and open output cell and header files 
 500   CONTINUE 
       call gtnam(0,8,dsnam,map,ier)
       CALL DSOPN(ICHOUT,MAP,3,3,1,2,128,IER) 
       IF(IER.NE.0) GO TO 90500 
C 
       call gtnam(0,5,dsnam,MAP,IER) 
       CALL DSOPN(ICHAN5,MAP,3,3,1,256,256,IER) 
       IF(IER.NE.0) GO TO 90500 
C 
C 
C     Open channel to output data file 
C 
        rewind(izfil)
C       OPEN ( UNIT=IZFIL,status='SCRATCH',FORM='UNFORMATTED', 
C     +        ACCESS='DIRECT',RECL=4,IOSTAT=IER,ERR=91800 ) 
C 
C     Loop to read the interpolated values and write it to cell file 
       ZMAX  = -BIG 
       ZVMAX = -BIG 
       ZMIN  =  BIG 
       ZVMIN =  BIG 
       DO 4000 IROW=1,MAXR 
         YCELL=YMAX + (FLOAT(IROW-1)*YSIDE) + (YSIDE/2.0) 
         DO 2000 ICOL=1,MAXC 
           CELL = (FLOAT(IROW-1) * MAXC) + ICOL 
           XCELL=XMIN + (FLOAT(ICOL-1)*XSIDE) + (XSIDE/2.0) 
           VAL = DEFLT 
           VAR = DEFLT 
C 
C        If this cell is masked, skip it 
           IF (ICHM .EQ. 0)  GO TO 1200 
             CALL DSIN(ICHM,IMASK,CELL,IER) 
             IF (IER .NE. 0) GO TO 90800 
             IF (MASK .NE. 0) GO TO 1800 
 1200      CONTINUE 
C 
C        Read a record 
        IWTF = IWTF + 1 
        READ ( IZFIL, REC = IWTF ) VAL 
C 
C        Write it out 
 1800      CONTINUE 
           ZMIN = AMIN1(VAL,ZMIN) 
           ZMAX = AMAX1(VAL,ZMAX) 
           CALL DSOUT(ICHOUT,IVAL,CELL,IER) 
           IF  (IER .NE. 0) GO TO 91000 
 2000    CONTINUE 
 4000  CONTINUE 
C 
C     Write header now that max and min values are known 
C 
C       BYTE (IHEAD,175) = BYTE (ZMIN,1) 
C       BYTE (IHEAD,176) = BYTE (ZMIN,2) 
C       BYTE (IHEAD,177) = BYTE  (ZMIN,3) 
C       BYTE (IHEAD,178) = BYTE (ZMIN,4) 
C       BYTE (IHEAD,179) = BYTE (ZMAX,1) 
C       BYTE (IHEAD,180) = BYTE (ZMAX,2) 
C       BYTE (IHEAD,181) = BYTE (ZMAX,3) 
C       BYTE (IHEAD,182) = BYTE (ZMAX,4) 
       IHEAD(88) = IZMIN(1) 
       IHEAD(89) = IZMIN(2) 
       IHEAD(90) = IZMAX(1) 
       IHEAD(91) = IZMAX(2) 
       CALL DSOUT(ICHAN5,IHEAD,1.0,IER) 
       IF (IER .NE. 0) GO TO 91200 
C 
C     Close output files 
C 
 5000  CALL DSCLS(ICHAN5,IER) 
       CALL DSCLS(ICHOUT,IER) 
       IF (ICHM .NE. 0) CALL DSCLS(ICHM,IER) 
C 
C     Update directory and write it to data base 
      CALL TUPDIR(ITP1,ITP2,ITP3,holdit,IHEAD,iwork,2,IER) 
      CLOSE(2,IOSTAT=IER) 
C 
C     Clean up 
C 
C      DELETE "TEMPGRID" 
       CLOSE(IZFIL) 
 9999 RETURN 
C 
C ********************************************************************* 
C 
C  ERROR RETURNS 
C 
90050 WRITE (NPRNT,90055) IER 
90055 FORMAT (' ***CUBSPLIN*** ERROR ',I5,' FROM WRXYZ') 
      GO TO 9999 
90100 WRITE (NPRNT,90105) IER 
90105 FORMAT (' ***CUBSPLIN*** ERROR ',I5,' FROM AKIMA.F77') 
      GO TO 9999 
90400 WRITE (NPRNT,90405) IER 
90405 FORMAT (' ***CUBSPLIN*** ERROR ',I5, 
     1 ' FROM ATTEMPT TO OPEN MASK MAP ') 
      GO TO 9999 
90500 WRITE (NPRNT,90505) IER 
90505 FORMAT (' ***CUBSPLIN*** ERROR ',I5, 
     1 ' FROM ATTEMPT TO OPEN OUTPUT MAP ') 
      GO TO 9999 
90700 WRITE (NPRNT,90705) IER 
90705 FORMAT ('***CUBSPLIN*** ERROR ',I5, 
     1' FROM OPEN OF STATPAC .UGR FILE ') 
      GO TO 9999 
90800 WRITE (NPRNT,90805) IER 
90805 FORMAT (' ***CUBSPLIN*** ERROR ',I5, 
     1' FROM ATTEMPT TO READ MASK MAP', 
     2       /' ROW ',I5,' COLUMN ',I5) 
      GO TO 9999 
90900 WRITE (NPRNT,90905) IER,IROW,ICOL 
90905 FORMAT (' ***CUBSPLIN*** ERROR ',I5,' FROM ATTEMPT TO READ ' 
     1      ,'TEMPGRID FILE',/,' ROW ',I5,' COLUMN ',I5) 
      GO TO 9999 
91000 WRITE (NPRNT,91005) IER,IROW,ICOL 
91005 FORMAT(' ***CUBSPLIN*** ERROR ',I5,' 
     1FROM ATTEMPT TO WRITE CELL FILE', 
     2      /' ROW ',I5,' COLUMN ',I5) 
      GO TO 9999 
91200 WRITE (NPRNT,91205) IER 
91205 FORMAT ('***CUBSPLIN*** ERROR ',I5,' 
     1FROM ATTEMPT TO WRITE HEADER ') 
      GO TO 9999 
91400 WRITE (NPRNT,91405) IER 
91405 FORMAT (' ***CUBSPLIN*** ERROR ',I5,' FROM OPEN OF NAMES FILE ') 
      GO TO 9999 
91600 WRITE (NPRNT,91605) IER 
91605 FORMAT (' ***CUBSPLIN*** ERROR ',I5,' FROM OPEN OF PARAM FILE ') 
      GO TO 9999 
91800 WRITE (NPRNT,91805) IER 
91805 FORMAT (' ***CUBSPLIN*** ERROR ',I5, 
     1 ' FROM OPEN OF TEMPGRID FILE ') 
      GO TO 9999 
      END 
