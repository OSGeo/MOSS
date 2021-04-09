C******************************************************************************
C
C     NAME:  SUBROUTINE GRIDMK
C
C     FUNCTION:  THIS ROUTINE USES A FILE IN CONJUCTION WITH
C                GRID INTERPOLATION TO PRODUCE A CELL FILE.  THE FILE
C                CONTAINS CELL VALUES,THE X AND Y COORDINATES AND THE
C                ELEVATION POINT.  THE INTERPOLATION OPTION IS PASSED TO
C                THIS ROUTINE.
C
C     PARAMETERS:  NAME     USE     TYPE     DIMENSION     RANGE
C                  -------------------------------------------------
C                  IOPT     I      I*2                    +INTEGER
C                  GRIDDING OPTION 1-4 POINT QUADRANT WEIGHTED AVERAGE
C                                  2-8 NEAREST POINTS WEIGHTED AVERAGE
C                                  3-FOR SIMPLE OR UNIVERSAL KRIGING
C                                  5-FOR NEAREST NEIGHBOR
C                  -------------------------------------------------
C                  NPTS     I      I*2                    +INTEGER
C                  NUMBER OF ROW,COLUMN AND X,Y,ELEVATION VALUES
C                  -------------------------------------------------
C                  MOVWIN   I      I*2                    +INTEGER
C                  SIZE OF THE ROVING WINDOW MATRIX
C                  -------------------------------------------------
C                  DEFLT    I      R*4                     REAL
C                  THE VALUE TO ASSIGN TO MASKED CELLS
C                  -------------------------------------------------
C                  MAXC     I      I*2                     +INTEGER
C                  NUMBER OF COLUMNS IN THE NEW CELL FILE
C                  -------------------------------------------------
C                  MAXR     I      I*2                     +INTEGER
C                  NUMBER OF ROWS IN THE NEW CELL FILE
C                  -------------------------------------------------
C                  AWIND    I     REAL ARRAY      4        +REAL
C                  THE MBR (XMIN, XMAX, YMIN, YMAX) FOR THE NEW FILE
C                  -------------------------------------------------
C                  XSIDE     I      R*4                    +REAL
C                  WIDTH OF CELL
C                  -------------------------------------------------
C                  YSIDE     I      R*4                    +REAL
C                  HEIGHT OF CELL
C                  -------------------------------------------------
C                  ICHRCZ    I      I*2                     +INTEGER
C                  CHANNEL TO THE ROW COLUMN FILE BEING BUILT
C                  -------------------------------------------------
C                  ICHM      I      I*2                     +INTEGER
C                  CHANNEL TO THE MASK CELL FILE
C                  -------------------------------------------------
C                  LENM      I      I*2                     +INTEGER
C                  LENGTH IF THE MASK CELL FILE VALUES
C                  -------------------------------------------------
C                  ICHOUT    I      I*2                     +INTEGER
C                  CHANNEL TO THE NEW CELL FILE BEING BUILT
C                  -------------------------------------------------
C                  ZMIN      O      R*4                     REAL
C                  THE NEW CELL MAPS MINIMUM ELEVATION
C                  -------------------------------------------------
C                  ZMAX      O      R*4                     REAL
C                  THE NEW CELL MAPS MAXIMUM ELEVATION
C                  -------------------------------------------------
C                  IER       O      I*2                     INTEGER
C                  ERROR INDICATOR 1-NO ERROR
C                  -------------------------------------------------
C
C     FILES:  CELL,X,Y, ELEVATION FILE
C             MASK FILE
C             CELL FILE
C
C     ROUTINES REQUIRED:  DSIN  DSOUT  GRIDI1  GRIDI2  FBISEA
C                         APGET FAULT  FDATA
C
C     CALLING ROUTINES:  GRID
C
C******************************************************************************
C
C
       SUBROUTINE GRIDMK(IOPT,NPTS,MOVWIN,DEFLT,MAXC,MAXR,AWIND,XSIDE,
     +                 YSIDE,ICHRCZ,ICHM,ICHOUT,ZMIN,ZMAX,ibw,lenm,IER)
C
       COMMON /GWINDO/ BAND(3,50)
       COMMON /FLTDAT/ MAPIDF,ICHF,ISTAR,ISTOP,NFAULTS,IFDAT(2,64)
       COMMON /RELTN/  JELTN(50,4), NPOINT(64), NFLAG, NPTS1
C
       DIMENSION   IMASK(2), IRCZ(8),icell(2),izcell(2)
       REAL        AWIND(4), RCZ(4), CELWIN(4), RVMBR(4),SMVAL
       LOGICAL     FLTMAP,ALONE
       EQUIVALENCE (IMASK(1),RMASK),(RCZ(1),IRCZ(1)),
     + (icell(1),cell),(izcell(1),zcell)
       DATA SMVAL /.00001/
C
      MAXPTS = 50
      write(*,*)
      XMIN = AWIND(1)
      YMAX = AWIND(4)
C
C.... (XCELL,YCELL) IS THE CENTER OF THE TARGET CELL.
C.... CELWIN(4) IS THE MBR FOR THE TARGET CELL
C
      IPTS = 2
      FLTMAP = .FALSE.
      IF (NFLAG .NE. 0) FLTMAP = .TRUE.
      IF (.NOT. FLTMAP) GO TO 5
        IPTS = 4
        CALL APGET(MAPIDF,ISTAR,ISTOP,NFAULTS)
        IF (NFAULTS .GT. 64) GO TO 85020
        WRITE(*,2) NFAULTS
    2   FORMAT(/,'  ***  SAMPLE FAULTS HAVE BEEN READ IN  ***',/,
     +           '       NUMBER OF SAMPLE FAULTS IS ',I2,/)
        CALL FDATA(IER)
C
    5 KSER = 8
      IF (IOPT .EQ. 5)  KSER = 1 
      DEFNUM = 0.0
      ZMIN = 9999999.
      ZMAX = - ZMIN
      CELLCT = FLOAT(MAXR) * FLOAT(MAXC)
C
C.....INITIALIZE THE CELL FILE
      CELL = 0.0
      recn=4.0
      CALL DSOUT(ICHOUT,ICELL(1),recn,IER)
      IF (IER .ne. 0) GO TO 85060
C
C.....PROCESSES ROW BY ROW
C
      DO 4000 IROW = 1,MAXR
        YCELL = YMAX - (FLOAT(IROW-1) * YSIDE) - (YSIDE/2.)
        CELWIN(2) = YMAX - ( FLOAT(IROW-1) * YSIDE )
        CELWIN(1) = CELWIN(2) - YSIDE
C
C.......EXPAND CELL WINDOW FOR CHEATING ON CONTAINMENT
        IF (IROW .EQ. 1   ) CELWIN(1) = CELWIN(1) + (YSIDE / 2.)
        IF (IROW .EQ. MAXR) CELWIN(1) = CELWIN(1) - (YSIDE / 2.)
C
C.......PROCESS COLUMN BY COLUMN .....
C
        DO 2000 ICOL=1,MAXC
          CELL = (FLOAT(IROW-1) * MAXC) + ICOL
          XCELL = XMIN + (FLOAT(ICOL-1) * XSIDE) + (XSIDE/2.0)
          ZCELL = DEFLT
          CELWIN(3) = XMIN + ( FLOAT(ICOL-1) * XSIDE )
          CELWIN(4) = XMIN + ( FLOAT(ICOL) * XSIDE )
C.........EXPAND CELL WINDOW FOR CHEATING CONTAINMENT
          IF (ICOL .EQ. 1) CELWIN(4) = CELWIN(4) - (XSIDE / 2.)
          IF (ICOL .EQ. MAXC) CELWIN(4) = CELWIN(4) + (XSIDE / 2.)
C
C.........IS THIS CELL MASKED  IF SO THEN SKIP IT
          IF (ICHM .EQ. 0)  GO TO 1200
            CALL DSIN(ICHM,IMASK,CELL,IER)
            IF (IER .ne. 0) GO TO 85040
            ZCELL = DEFLT
C            IF (ABS(RMASK) .GT. 0.0) THEN
C               ZCELL = RMASK
C               GO TO 1800
C            END IF
            IF ( LENM.EQ.1 ) THEN
              IF ( ABS(IMASK(1)).GT.0 ) ZCELL = FLOAT(IMASK(1))
              GO TO 1800
            ELSE
              IF ( ABS(RMASK).GT.0. ) ZCELL = RMASK
              GO TO 1800
            END IF
C
C.........FIND THE VALUES WITHIN THE MOVING WINDOW PROXIMITY OF THIS CELL
C
 1200     CONTINUE
          NR = 0
C........ GWF: JUL 88
C 1220     NPTS1 = 0
 1220     NR = NR + 1
C          IF ( NR.GT.MOVWIN ) GO TO 1800
          IF ( IOPT.EQ.1 .AND. NR.GT.MOVWIN .AND. NPTS1.LE.0 )
     +         GO TO 1800
          IF ( IOPT.EQ.1 .AND. NR.GT.MOVWIN .AND. NPTS1.GT.0 )
     +         GO TO 1650
          IF ( IOPT.EQ.2 .AND. NR.GT.MOVWIN .AND. NPTS1.LE.2 )
     +         GO TO 1800
          IF ( IOPT.EQ.2 .AND. NR.GT.MOVWIN .AND. NPTS1.GT.2 )
     +         GO TO 1650
          IF ( IOPT.EQ.5 .AND. NR.GT.MOVWIN .AND. NPTS1.LE.0 )
     +         GO TO 1800 
          IF ( IOPT.EQ.5 .AND. NR.GT.MOVWIN .AND. NPTS1.GT.0 )
     +         GO TO 1650 
C
            NPTS1 = 0
C........ GWF: JUL 88 END
            IR1 = IROW - NR
            IR2 = IROW + NR
            IC1 = ICOL - NR
            IC2 = ICOL + NR
            IF (IR1 .LT. 1)    IR1 = 1
            IF (IR2 .GT. MAXR) IR2 = MAXR
            IF (IC1 .LT. 1)    IC1 = 1
            IF (IC2 .GT. MAXC) IC2 = MAXC
            RVMBR(1) = XMIN + (FLOAT(IC1-1) * XSIDE)
            RVMBR(2) = XMIN + (FLOAT( IC2 ) * XSIDE)
            RVMBR(3) = YMAX - (FLOAT( IR2 ) * YSIDE)
            RVMBR(4) = YMAX - (FLOAT(IR1-1) * YSIDE)
C
            CLMIN = FLOAT (((IR1 - 1) * MAXC) + IC1)
            CLMAX = FLOAT (((IR2 - 1) * MAXC) + IC2)
            CALL FBISEA (NPTS,ICHRCZ,ioerr,CLMIN,1,8,
     +                   NEXACT,RCZ(1),NEXREC,IER)
            IF (NEXACT .GT. 0) NEXREC = NEXACT
            IF (NEXACT .GT. 0) GO TO 1300
              NEXREC = NEXREC - 1
C
 1250         NEXREC = NEXREC + 1
              IF (NEXREC .GT. NPTS) GO TO 1400
                CALL DSIN (ICHRCZ,IRCZ(1),FLOAT(NEXREC),IER)
                IF (IER .ne. 0) GO TO 85040
C
C...............IS CELL NUMBER IN CURRENT BOUNDS ?
                IF (RCZ(1) .LT. CLMIN) GO TO 1250
                IF (RCZ(1) .GT. CLMAX) GO TO 1400
C
C.................YES, BUT WHAT ABOUT COLUMN BOUNDS ?
                  ICLM = INT(AMOD(RCZ(1),FLOAT(MAXC)))
                  IF (ICLM .EQ. 0) ICLM = MAXC
                  IF (ICLM .LT. IC1 .OR. ICLM .GT. IC2) GO TO 1250
C
C.................IN COLUMN BOUNDS, BUT IS THIS A REDUNDANT POINT?
 1300             IF ( NPTS1.LT.1 ) GO TO 1330
                    DO 1325 M = 1, NPTS1
                      IF (ABS(BAND(1,M) - RCZ(2)) .LT. SMVAL .AND.
     +                    ABS(BAND(2,M) - RCZ(3)) .LT. SMVAL .AND.
     +                ABS(BAND(3,M) - RCZ(4)) .LT. SMVAL) GO TO 1250
 1325               CONTINUE
C
C.................NOT REDUNDANT - GOOD ONE, COUNT AND SAVE IT
 1330             IF ( NPTS1+1.GT.MAXPTS ) GO TO 1600
                    NPTS1 = NPTS1 + 1
                    DO 1350 M = 1, 3
                      BAND(M,NPTS1) = RCZ(M+1)
 1350               CONTINUE
                    GO TO 1250
C
C.............ENOUGH POINTS?? OR EXPAND WINDOW AND GO AGAIN
 1400         CONTINUE
C............ GWF:  JUL 88
C              IF ( NPTS1.LT.KSER ) GO TO 1220
              IF ( IOPT.EQ.1 .AND. NPTS1.LT.MAXPTS ) GO TO 1220
              IF ( IOPT.EQ.2 .AND. NPTS1.LT.KSER ) GO TO 1220
              IF ( IOPT.EQ.5 .AND. NPTS1.LT.KSER ) GO TO 1220 
C............ GWF:  JUL 88 END
C
C.............SOME POINTS WERE FOUND AROUND THIS CELL...COMPUTE VALUE
 1600         CONTINUE
C
C.............PROCESS FAULT LINES, IF THEY EXIST.
              IF (.NOT. FLTMAP)  GO TO 1650
                CALL FAULT(CELWIN,XCELL,YCELL,RVMBR,ALONE,IER)
                IF (IER .ne. 0) GO TO 85080
C
C...............IF ISOLATED TARGET WAS FOUND, SET ZCELL.
C...............DO NOT INTERPOLATE ANY VALUES..
                IF (.NOT. ALONE) GO TO 1650
                  ZCELL = DEFLT
c                  WRITE(*,1647)  IROW,ICOL
c 1647             FORMAT(' TARGET CELL IN ROW ',I3,'  COLUMN ',I3,
c     +                   ' IS ISOLATED.')
                  GO TO 1800
C
 1650         CONTINUE
              IF (IOPT .EQ. 1)
     +            CALL GRIDI1(XCELL,YCELL,NPTS1,ZCELL,CELWIN,IER)
C............ GWF:  JUL 88.  TELL GRIDI2 THE NUMBER OF POINTS TO CONSIDER
C............       KKSER WILL RANGE FROM KSER(=3) TO 8
              IF (IOPT .EQ. 2) THEN
                 KKSER = MAX ( MIN ( 8, NPTS1 ), KSER )
                 CALL GRIDI2(XCELL,YCELL,KKSER,ZCELL,CELWIN,IER)
              END IF
              IF ( IOPT .EQ. 5 ) THEN
                 CALL GRIDI2(XCELL,YCELL,KSER,ZCELL,CELWIN,IER)
              ENDIF
              IF (IER .ne. 0) GO TO 80000
C
C...........OUTPUT THE COMPUTED CELL VALUE .....
 1800       CONTINUE
C.......... GWF:  JUL 88 COUNT DEFAULT ASSIGNMENTS
            IF ( ABS(ZCELL-DEFLT).LT.SMVAL ) DEFNUM = DEFNUM + 1
            ZMIN = MIN(ZCELL,ZMIN)
            ZMAX = MAX(ZCELL,ZMAX)
            CALL DSOUT(ICHOUT,IZCELL(1),CELL,IER)
            IF  (IER .ne. 0) GO TO 85060
        WRITE(*,90160) icol,maxc,IROW,maxr
 2000   CONTINUE
90160 FORMAT('+',' Processing Col.',i5,' of',i5,' in Row',i5,' of',i5)
 4000 CONTINUE
C.... GWF:  JUL 88.  DEFAULT ASSIGNMENTS' REPORT IF NECESSARY
      IF ( DEFNUM.GT.0 ) WRITE(*,4009) DEFNUM, DEFLT
 4009 FORMAT(' ',F15.0,' CELLS WERE ASSIGNED BACKGROUND VALUE ',F15.3)
C
C.....NORMAL RETURN
      GO TO 90000
C
C.....ERROR RETURNS
C
80000 WRITE(*,98000) IOPT,IER
98000 FORMAT(' *GRIDMK*  ERROR FROM GRIDI',I1,'  ERROR',I5)
      GO TO 90000
C
85020 WRITE(*,98502)
98502 FORMAT(' *GRIDMK* ERROR - TOO MANY FAULTS - MAX OF 64 ')
      GO TO 90000
C
85040 WRITE(*,98504) IER
98504 FORMAT(' *GRIDMK*  ERROR FROM DSIN  ERROR',I5)
      GO TO 90000
C
85060 WRITE(*,98506) IER
98506 FORMAT(' *GRIDMK*  ERROR FROM DSOUT  ERROR',I5)
      GO TO 90000
C
85080 WRITE(*,98508) IER,IROW,ICOL
98508 FORMAT(' *GRIDMK*  ERROR FROM FAULT - ERROR',I5,
     +       ' ROW & COL = ',2I5)
C
90000 CONTINUE
      RETURN
      END
