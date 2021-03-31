C************************************************************************
C
C     NAME:  SUBROUTINE GRIDPT
C
C     FUNCTION:
C       PRODUCE A FLAT FILE, SORTED BY CELL NUMBER, WHICH CONTAINS THE
C       CELL NUMBVER, X, Y AND Z VALUES OF EACH INPUT POINT.  THIS FILE
C       IS ACCESSED BY GRIDMK FOR CELL VALUE INTERPOLATION.
C
C     PARAMETERS:  NAME     USE     TYPE     DIMENSION     RANGE
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
C                  MWORDS    I      I*2                    +INTEGER
C                  LENGTH OF ATTRIBUTE FIELD IN WORDS
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
C                  CHANNEL TO THE POINT MAP
C                  -------------------------------------------------
C                  ICHAT     I      I*2                     +INTEGER
C                  CHANNEL TO THE MULTIPLE ATTRIBUTE FILE
C                  -------------------------------------------------
C                  ICHRCZ    I      I*2                     +INTEGER
C                  CHANNEL TO THE ROW COLUMN FILE BEING BUILT
C                  -------------------------------------------------
C                  NROWS     I      I*2                    +INTEGER
C                  NUMBER OF ROWS IN CELL MAP
C                  -------------------------------------------------
C                  NCOLS     I      I*2                    +INTEGER
C                  NUMBER OF COLUMNS IN CELL MAP
C                  -------------------------------------------------
C                  NPTS      I      I*2                    +INTEGER
C                  NUMBER OF ROW,COLUMN AND X,Y,ELEVATION VALUES
C                  -------------------------------------------------
C                  ZZMIN     R      R*4                    +REAL
C                  MINIMUM Z VALUE OF INPUT POINTS
C                  -------------------------------------------------
C                  IER       O      I*2                     INTEGER
C                  ERROR INDICATOR 1-NO ERROR
C                  -------------------------------------------------
C
C     FILES:  POINT FILE
C             MULTIPLE ATTRIBUTE FILE
C             CELL,X,Y,ELEVATION FILE
C
C
C***********************************************************************
C
       SUBROUTINE GRIDPT(MAPID,IDTYPE,IATYP,MWORDS,LOCATE,
     +                   WIND,SCALEZ,AWIND,XSIDE,YSIDE,
     +                   ICHAN1,ICHAT,ICHRCZ,NROWS,NCOLS,NPTS,ZZMIN,IER)
$INCLUDE : 'IO.INC'
C       COMMON /IO/   NPRNT, IOIN
        COMMON /WORK/ IBUFF(128),MAP(25),ICOM(10),JCOM(10)
C
       INTEGER ISUB(128),IRCZ(8),IRCZ2(8)
       REAL WIND(4),FWIND(4),AWIND(4),XYZ(3),RCZ2(4)
       EQUIVALENCE (CLL,IRCZ(1)),(XYZ(1),IRCZ(3)),
     +             (RCZ2(1),IRCZ2(1))
C
       ZZMIN = 9999999.
       IRECL=256
       IRECLZ=16
C
C......GET BOUNDS ON MAPID POINTER SET .....
C
       call apget(MAPID,ISTART,ISTOP,NPTS)
C
C......SET NUMBER OF POINTS .....
C
       NPTS=0
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
     +       FWIND(3) .GT. WIND(4)  .OR.  FWIND(4) .LT. WIND(3) )
     +       GO TO 2000
C
C........GET FEATURE RECORD .....
C
         RECN=INDEX
         call rdblk(ichan1,index,ibuff,1,ier)
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
        RECN=INDEX
           call rdblk(ichan1,index,ibuff,1,ier)
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
        RECN=ISUBP
C           CALL DSIN(ICHAN1,ISUB,RECN,IER)
           call rdblk(ichan1,isubp,isub,1,ier)
           IF (IER .NE. 0) GO TO 85040
           GO TO 1660
C........ELEVATION IN MULTIPLE ATTRIBUTE FILE .....
 1640    CONTINUE
         ITEMN = IBUFF(5)
           CALL GETAT(ICHAT,IATYP,MWORDS,LOCATE,IBUFF,ITEMN,ISUB,
     +                RNUM,IER)
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
C........COMPUTE ROW(MAX-POSITION) AND COLUMN(MINIMUM-POSITION) LOCATION .....
C
 1800    CONTINUE
         IR = AINT((AWIND(4)-XYZ(2))/YSIDE)+1
         IC = AINT((XYZ(1)-AWIND(1))/XSIDE)+1
         IF (IR .LT. 1) IR = 1
         IF (IC .LT. 1) IC = 1
         IF (IR .GT. NROWS) IR = NROWS
         IF (IC .GT. NCOLS) IC = NCOLS
C
C........STORE CELL #, X, Y AND ELEVATION .....
C
      klen = 1
      ndim = 8
      ioerr = 10
        XYZ(3) = XYZ(3) * SCALEZ
        ZZMIN = MIN(ZZMIN,XYZ(3))
        CLL = ( ( FLOAT(IR-1) * FLOAT(NCOLS) ) + FLOAT(IC) )
        CALL FBISEA ( NPTS, ICHRCZ, ioerr, CLL, klen, ndim, NPTS1,
     +              RCZ2(1), INSERT, IER )
        NPTS = NPTS + 1
        IF ( NPTS1.EQ.0 ) NPTS1 = INSERT
        IF ( NPTS.EQ.NPTS1 ) GO TO 90
        DO 80 M = (NPTS-1), NPTS1, -1
          RECN=M
          CALL DSIN ( ICHRCZ, IRCZ2(1), RECN, IER )
          IF ( IER.NE.0 ) GO TO 85040
          RECN=M+1
          CALL DSOUT ( ICHRCZ, IRCZ2(1), RECN, IER )
          IF ( IER.NE.0 ) GO TO 85060
   80   CONTINUE
   90   RECN=NPTS1
        CALL DSOUT ( ICHRCZ, IRCZ(1), RECN, IER )
        IF ( IER.NE.0 ) GO TO 85060
 2000  CONTINUE
C
C......NORMAL RETURN .....
C
       GO TO 90000
C
C......ERROR RETURNS .....
C
80000  CONTINUE
       WRITE(NPRNT,98000) IER
98000  FORMAT(' *GRIDPT*  ERROR FROM GETFAST  ERROR',I5)
       GO TO 90000
C
80020  CONTINUE
       IER = 2
       WRITE(NPRNT,98002) IDTYPE,IER
98002  FORMAT(' *GRIDPT*  ILLEGAL DATA TYPE TYPE ',I5,'  ERROR',I5)
       GO TO 90000
C
85040  CONTINUE
       WRITE(NPRNT,98504) IER
98504  FORMAT(' *GRIDPT*  ERROR FROM DSIN  ERROR',I5)
       GO TO 90000
C
85060  CONTINUE
       WRITE(NPRNT,98506) IER
98506  FORMAT(' *GRIDPT*  ERROR FROM DSOUT  ERROR',I5)
C
C......JUMP OUT .....
C
90000  CONTINUE
       RETURN
       END
