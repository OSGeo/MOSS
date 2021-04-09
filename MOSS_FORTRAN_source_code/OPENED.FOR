C************************************************************************
C
C  Name:    O P E N E D
C
C  Purpose: FINDS SEG WITH SMALLEST ANGLE TO HORIZONTAL AND DETERMINES POINTS
C           FOR LABELING
C
C  Commons/Variables    In/Out        Attributes   Function/Size
C
C  -- Dummy Arguments --
C     IER               Out           Integer - Returned error code
C
C  -- IO --
C     NPRNT                           Integer
C     IOIN                            Integer
C
C  -- LOCO1 --
C     BOX                             Real Array            32
C     LENGTH                          Real
C     HGT                             Real
C     SMDIF                           Real
C     NSTART                          Integer
C     NSTOP                           Integer
C     IDO                             Integer
C     LOSER                           Integer
C     NBOX                            Integer
C     MAXBOX                          Integer
C     IATT                            Integer
C     IATYP                           Integer
C     MWORDS                          Integer
C     LOCATE                          Integer
C     NVERT                           Integer
C     NCHAR                           Integer
C     LREF                            Integer Array          4
C     MREF                            Integer Array          4
C
C  -- LOCO2 --
C     XMIN                            Real
C     XMAX                            Real
C     YMIN                            Real
C     YMAX                            Real
C     XMID                            Real
C     YMID                            Real
C     IQUAD                           Integer Array          4
C
C  -- Stack Variables --
C     FIRST                           Logical
C     GOOD1                           Logical
C     IQ                              Integer
C     I                               Integer
C     INC                             Integer
C     IB                              Integer
C     IR                              Integer
C     ISKIP                           Integer
C     LOCAS                           Integer Array         10
C     QXMIN                           Real
C     QXMAX                           Real
C     QYMIN                           Real
C     QYMAX                           Real
C     BEST                            Real
C     SEGLEN                          Real
C     THETA                           Real
C
C******************************************************************************
C
      SUBROUTINE OPENED ( IER )
C
$INCLUDE: 'XYPNTS.INC'
       COMMON /IO/     NPRNT,IOIN
       COMMON /LOCO1/  BOX(4,4), LENGTH, HGT, SMDIF, NSTART, NSTOP,IDO,
     +         LOSER, NBOX, MAXBOX, IATT, IATYP, MWORDS, LOCATE, NVERT,
     +         NCHAR, LREF(4), MREF(4)
       COMMON /LOCO2/  XMIN, XMAX, YMIN, YMAX, XMID, YMID, IQUAD(4)
C
       DIMENSION LOCAS(10)
       REAL      LENGTH
       LOGICAL   FIRST, GOOD1
C
      IER = 0
C
      IF (NVERT .LT. 2) GO TO 900
C
C.....LOOP THROUGH COORDINATES FOR ALL QUADRANTS INVOLVED WITH FEATURE
C
      DO 500 IQ = 1, 4
        IF ( IQUAD(IQ).EQ.0 ) GO TO 500
C.......SETUP QUADRANT WINDOW
        IF ( IQ.NE.1 ) GO TO 10
          QXMIN = XMID
          QXMAX = XMAX
          QYMIN = YMID
          QYMAX = YMAX
          GO TO 40
   10   IF ( IQ.NE.2 ) GO TO 20
          QXMIN = XMIN
          QXMAX = XMID
          QYMIN = YMID
          QYMAX = YMAX
          GO TO 40
   20   IF ( IQ.NE.3 ) GO TO 30
          QXMIN = XMIN
          QXMAX = XMID
          QYMIN = YMIN
          QYMAX = YMID
          GO TO 40
   30   IF ( IQ.NE.4 ) GO TO 40
          QXMIN = XMID
          QXMAX = XMAX
          QYMIN = YMIN
          QYMAX = YMID
C
   40   IR = NBOX + 1
C
C.......CYCLE THROUGH ALL POINTS FOR BEST STARTING POSITION FOR EACH QUADRANT
C.......MAKE COMPARISONS BASED ON RELATIVE MAGNITUDE
        INC = 0
   45   INC    = INC + 1
C.......IF SKIPPING OVER HALF OF FEATURES COORDINATES, SPLIT WITHOUT LABELING
        IF ( INC.GT.NVERT/2 ) GO TO 500
          IB = 0
          BEST = 10.
C.........GOOD1 = GOOD LOCATION
C.........FIRST = FIRST OCCURANCE INSIDE CURRENT QUADRANT
          GOOD1 = .FALSE.
          FIRST = .FALSE.
          DO 60 I=INC+1,NVERT,INC
            ISKIP = 0
C
             call spoints('X ',x,xi,i,1)
             call spoints('Y ',y,yi,i,1)
             call spoints('X ',x,xiinc,i-inc,1)
             call spoints('Y ',y,yiinc,i-inc,1)
            IF ( XI.LE.XIINC .AND. YI.LE.YIINC )
     +           ISKIP = INVWND ( XI, XIINC, YI, YIINC,
     +                             QXMIN,  QXMAX,  QYMIN, QYMAX )
            IF ( ISKIP.EQ.1 ) GO TO 50
C
            IF ( XI.LE.XIINC .AND. YI.GT.YIINC )
     +           ISKIP = INVWND ( XI, XIINC, YIINC, YI,
     +                             QXMIN,  QXMAX,  QYMIN, QYMAX )
            IF ( ISKIP.EQ.1 ) GO TO 50
C
            IF ( XI.GT.XIINC .AND. YI.LE.YIINC )
     +           ISKIP = INVWND ( XIINC, XI, YI, YIINC,
     +                             QXMIN,  QXMAX,  QYMIN, QYMAX )
            IF ( ISKIP.EQ.1 ) GO TO 50
C
            IF ( XI.GT.XIINC .AND. YI.GT.YIINC )
     +           ISKIP = INVWND ( XIINC, XI, YIINC, YI,
     +                             QXMIN,  QXMAX,  QYMIN, QYMAX )
C...........0= NOT IN CURRENT QUAD; 1= AT LEAST TOUCHES CURRENT QUAD
   50       CONTINUE
            IF ( ISKIP.EQ.0 ) GO TO 60
              IF ( FIRST .AND. ISKIP.EQ.1 ) GO TO 55
                IF ( ISKIP.EQ.1 ) FIRST = .TRUE.
                IF ( ISKIP.EQ.1 ) GO TO 60
                GO TO 58
C.............SEGMENT LONG ENOUGH FOR LABEL?
   55         SEGLEN = SQRT(((XI-XIINC)**2) +
     +                      ((YI-YIINC)**2))
              SEGLEN = SEGLEN - ( SEGLEN / 15. )
C  FUDGE FACTOR
              IF ( SEGLEN.LT.LENGTH ) GO TO 60
C...............CALCULATE DEVIATION FROM HORIZONTAL UNLESS VERTICAL
                THETA = 1.570796327
                IF (ABS(XI - XIINC) .GE. 1.E-05)
     +              THETA = ATAN( ABS(YI - YIINC)/ABS(XI -
     +                            XIINC) )
C...............IF LEAST DEVIATION SO FAR, SAVE ANGLE AND POINTER
                IF (THETA .GE. BEST) GO TO 60
                  BEST = THETA
                  IF ( IB.LT.10 ) IB = IB + 1
                  LOCAS(IB) = I
                  GOOD1 = .TRUE.
                  GO TO 60
   58       FIRST = .FALSE.
   60     CONTINUE
C
          IF ( .NOT.GOOD1 ) GO TO 45
C
C.........PLACE LABEL AT ONE OF PRE-DETERMINED LOCATIONS
          DO 300 I = IB, 1, -1
            LREF(IR) = LOCAS(I) - INC
            MREF(IR) = LOCAS(I)
C
            CALL LIMIT ( IER )
            IF ( IER.NE.-1) GO TO 500
  300     CONTINUE
C
  500 CONTINUE
C
  900 IER = 0
      RETURN
      END
