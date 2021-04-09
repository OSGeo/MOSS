C************************************************************************
C
C  Name:    L B L B O X
C
C  Purpose: DECIDES WHETHER INPUT LINE IS OPEN OR CLOSED, COMPUTES LINE
C           LENGTH, ADJUSTS # OF LABELS REQUESTED (IF NECESSARY),CALLS
C           ROUTINES FOR MASKING COORDINATE DETERMINATION AND THEN WRITE
C           OUT NEW FEATURES TO TEMP FILES
C
C  Commons/Variables    In/Out        Attributes   Function/Size
C
C  -- Dummy Arguments --
C     LEBAL             In            Logical - Label/NoLabel flag
C     IER               Out           Integer - Returned error code
C
C  -- IO --
C     NPRNT                           Integer
C     IOIN                            Integer
C
C  -- ITEMMS --
C     ITEM1                           Integer
C     ITEM2                           Integer
C
C  -- OFFST --
C     XOFF                            Real
C     YOFF                            Real
C
C  -- OSAVE --
C     ISUB                            Integer Array         15
C     NPOLYS                          Integer
C     LTYP2                           Integer
C     IDISP                           Integer
C     NOSAVE                          Logical
C     ICTSUB                          Integer
C
C  -- WORK --
C     IBUFF                           Integer Array        128
C     MSNAME                          Integer Array         15
C     LCOM                            Integer Array         10
C     KCOM                            Integer Array         10
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
C  -- Static Variables --
C     ICAT                            Integer Array          5
C
C  -- Stack Variables --
C     SAVE                            Logical
C     I                               Integer
C     J                               Integer
C     NCOORS                          Integer
C     M                               Integer
C     ISTAR                           Integer
C     ISTOP                           Integer
C     IZER                            Integer
C     IFLAG                           Integer
C     ITEMP                           Integer
C     ICOORS                          Integer Array          4
C     YT                              Real
C     XT                              Real
C     XDIF                            Real
C     YDIF                            Real
C     TEMP                            Real
C
C  --   external Subprograms --
C     X              Y              CLOSED         WRSEQ          MOVEA
C     DRAWA          OPENED         Qanmode
C
C  Files:
C       Unit    File name or description
C       ------  -------------------------------------------------------
C       18      "TEMP"
C       29      "TEMPSUB"
C
C  Calling:          LBGENR
C******************************************************************************
C
      SUBROUTINE LBLBOX ( LEBAL,NVW, IER )
C
$INCLUDE: 'XYPNTS.INC'
       COMMON /IO/     NPRNT,IOIN
       COMMON /ITEMMS/ ITEM1, ITEM2
       COMMON /OFFST/  XOFF, YOFF
       COMMON /OSAVE/  ISUB(15),NPOLYS,LTYP2,IDISP,NOSAVE,ICTSUB,ITREC
       COMMON /WORK/   IBUFF(128),MSNAME(25),LCOM(10),KCOM(10)
       COMMON /LOCO1/  BOX(4,4), LENGTH, HGT, SMDIF, NSTART, NSTOP,
     +                 IDO, LOSER,NBOX, MAXBOX, IATT, IATYP, MWORDS,
     +                 LOCATE, NVERT, NCHAR,LREF(4), MREF(4)
       COMMON /LOCO2/  XMIN, XMAX, YMIN, YMAX, XMID, YMID, IQUAD(4)
C
       INTEGER ICOORS(4), ICAT(5)
       REAL    LENGTH
       LOGICAL NOSAVE, LEBAL, SAVE
       EQUIVALENCE ( XT, ICOORS(1) ), ( YT, ICOORS(3) )
       DATA ICAT/5*0/
C
       NVERT = NVW
       NBOX = 0
       IER = 0
       IZER = 0
       SAVE = .NOT.NOSAVE
C
C.....IF START AND STOP COORDS SAME THEN WE'RE DEALING WITH A
C.....CLOSED LOOP ->CONTINUE; ELSE GO TO LINE HANDLING AT 100
C
      SMDIF = AMIN1((XMAX-XMIN),(YMAX-YMIN)) / 25.
      call spoints('X ',x,x1,1,1)
      call spoints('X ',x,xnvert,nvert,1)
      XDIF  = ABS ( X1 - XNVERT )
      call spoints('Y ',y,y1,1,1)
      call spoints('Y ',y,ynvert,nvert,1)
      YDIF  = ABS ( Y1 - YNVERT )
C
      IF ( XDIF.GT.SMDIF .OR. YDIF.GT.SMDIF ) GO TO 100
C
        IF ( LEBAL ) CALL CLOSED ( IER )
C
C.....CREATE NEW FEATURE(S):  3 CASES
C.....ROOM FOR LABEL?
      IF ( NBOX.GT.0 ) GO TO 50
C.......NO, DUPLICATE FEATURE:  CASE 1
        IF ( NOSAVE ) GO TO 35
       ITREC = ITREC + 1
          WRITE(ICTSUB,REC=ITREC) (ISUB(I),I=1,15)
C    9     FORMAT(15A2)
          NPOLYS = NPOLYS + 1
       WRITE(18)(ISUB(II),II=1,15),(ICAT(II),II=1,5),NVERT,IZER
   35   DO 40 I = 1, NVERT
          call spoints('X ',x,xi,i,1)
          call spoints('Y ',y,yi,i,1)
          XT = XI + XOFF
          YT = YI + YOFF
          IF ( SAVE ) WRITE ( 18 )(ICOORS(II),II=1,4)
          IF ( SAVE ) GO TO 40
            IF ( I.EQ.1 ) CALL MOVEA ( XT, YT )
            IF ( I.NE.1 ) CALL DRAWA ( XT, YT )
   40   CONTINUE
        GO TO 900
C.....YES, MAKE NEW FEATURE(S)
   50 IF ( LREF(1).GT.MREF(1) ) NCOORS = LREF(1) - MREF(1) + 3
      IF ( LREF(1).LT.MREF(1) ) NCOORS = LREF(1) + 1
      IF ( NOSAVE ) GO TO 60
      ITREC = ITREC + 1
        WRITE(ICTSUB,REC=ITREC) (ISUB(I),I=1,15)
        NPOLYS = NPOLYS + 1
      WRITE(18)(ISUB(II),II=1,15),(ICAT(II),II=1,5),NCOORS,IZER
C.....WHICH CASE?
   60 IF ( LREF(1).LT.MREF(1) ) GO TO 75
C
C.......CASE 2:  1 FEATURE BECAUSE LABEL OVERLAPS BEGINNING AND END POINTS
        XT = BOX(1,1)
        YT = BOX(1,2)
        IF ( SAVE ) WRITE ( 18 )(ICOORS(II),II=1,4)
        IF ( NOSAVE  ) CALL MOVEA ( XT, YT )
        DO 70 I = MREF(1), LREF(1)
        call spoints('X ',x,xi,i,1)
        call spoints('Y ',y,yi,i,1) 
          XT = XI + XOFF
          YT = YI + YOFF
          IF ( SAVE ) WRITE ( 18 )(ICOORS(II),II=1,4)
          IF ( NOSAVE ) CALL DRAWA ( XT, YT )
   70   CONTINUE
        XT = BOX(1,3)
        YT = BOX(1,4)
        IF ( SAVE ) WRITE ( 18 )(ICOORS(II),II=1,4)
        IF ( NOSAVE ) CALL DRAWA ( XT, YT )
        GO TO 900
C
C.......CASE 3:  2 FEATURES BECAUSE LABEL BETWEEN START AND END POINTS
   75 DO 80 I = 1, LREF(1)
        call spoints('X ',x,xi,i,1)
        call spoints('Y ',y,yi,i,1)
        XT = XI + XOFF
        YT = YI + YOFF
        IF ( SAVE ) WRITE ( 18 )(ICOORS(II),II=1,4)
        IF ( SAVE ) GO TO 80
          IF ( I.EQ.1 ) CALL MOVEA ( XT, YT )
          IF ( I.NE.1 ) CALL DRAWA ( XT, YT )
   80 CONTINUE
      XT = BOX(1,1)
      YT = BOX(1,2)
      IF ( SAVE ) WRITE ( 18 )(ICOORS(II),II=1,4)
      IF ( NOSAVE ) CALL DRAWA ( XT, YT )
      IF ( NOSAVE ) GO TO 85
      ITREC = ITREC + 1
        WRITE(ICTSUB,REC=ITREC) (ISUB(I),I=1,15)
        NPOLYS = NPOLYS + 1
        NCOORS = NVERT - MREF(1) + 2
        WRITE(18)(ISUB(II),II=1,15),(ICAT(II),II=1,5),NCOORS,IZER
   85 XT = BOX(1,3)
      YT = BOX(1,4)
      IF ( SAVE ) WRITE ( 18 )(ICOORS(II),II=1,4)
      IF ( NOSAVE  ) CALL MOVEA ( XT, YT )
      DO 90 I = MREF(1), NVERT
      call spoints('X ',x,xi,i,1)
      call spoints('Y ',y,yi,i,1)
        XT = XI
        YT = YI
        IF ( SAVE ) WRITE ( 18 )(ICOORS(II),II=1,4)
        IF ( NOSAVE ) CALL DRAWA ( XT, YT )
   90 CONTINUE
      GO TO 900
C
C  DEALING WITH LINES, NOT LOOPS
C
  100 IF ( LEBAL ) CALL OPENED ( IER )
C
C.....SORT LABEL REFERENCE POINTERS FOR FEATURE BUILDING BASED ON FIRST
C.....OCCURANCE WITHIN FEATURE
      IF ( NBOX.LT.2 ) GO TO 255
  200   IFLAG = 0
        DO 250 I = 1, NBOX-1
          IF ( LREF(I).LT.LREF(I+1) ) GO TO 250
C...........INTERCHANGE LEFT COORD PNTRS
            ITEMP     = LREF( I )
            LREF( I ) = LREF(I+1)
            LREF(I+1) = ITEMP
C...........INTERCHANGE RIGHT COORD PNTRS
            ITEMP     = MREF( I )
            MREF( I ) = MREF(I+1)
            MREF(I+1) = ITEMP
C...........INTERCHANGE LABEL COORDINATES
            DO 225 J = 1, 4
              TEMP       = BOX( I ,J)
              BOX( I ,J) = BOX(I+1,J)
              BOX(I+1,J) = TEMP
  225       CONTINUE
C...........FLAG EXCHANGE
            IFLAG = 1
  250   CONTINUE
        IF ( IFLAG.NE.0 ) GO TO 200
C
C.....CREATE NEW FEATURES IN TEMP FILES
  255 M = 0
      IF ( NBOX.GT.0 ) GO TO 300
C.......NO ROOM FOR LABEL, DUPLICATE FEATURE
        IF ( NOSAVE ) GO TO 260
       ITREC = ITREC + 1
          WRITE(ICTSUB,REC=ITREC) (ISUB(I),I=1,15)
          NPOLYS = NPOLYS + 1
       WRITE(18)(ISUB(II),II=1,15),(ICAT(II),II=1,5),NVERT,IZER
  260   DO 280 I = 1, NVERT
          call spoints('X ',x,xi,i,1)
          call spoints('Y ',y,yi,i,1)
          XT = XI + XOFF
          YT = YI + YOFF
          IF ( SAVE ) WRITE ( 18 )(ICOORS(II),II=1,4)
          IF ( SAVE  ) GO TO 280
            IF ( I.EQ.1 ) CALL MOVEA ( XT, YT )
            IF ( I.NE.1 ) CALL DRAWA ( XT, YT )
  280   CONTINUE
        GO TO 900
C
C.....ROOM FOR LABEL(S), 1 LABEL -> 2 FEATURES;  4 LABEL -> 5 FEATURES
  300 M = M + 1
      IF ( SAVE ) THEN
       ITREC = ITREC + 1
       WRITE(ICTSUB,REC=ITREC) (ISUB(I),I=1,15)
          NPOLYS = NPOLYS + 1
      ENDIF
      ISTAR = 1
      IF ( M.GT.1 ) ISTAR = MREF(M-1)
      ISTOP = LREF(M)
      IF ( M.GT.NBOX ) ISTOP = NVERT
      NCOORS = ISTOP - ISTAR + 1
      IF ( ISTAR.EQ.1 ) NCOORS = NCOORS + 1
      IF ( ISTOP.EQ.NVERT ) NCOORS = NCOORS + 1
      IF ( ISTAR.NE.1 .AND. ISTOP.NE.NVERT ) NCOORS = NCOORS + 2
      IF ( NOSAVE ) GO TO 310
      WRITE(18)(ISUB(II),II=1,15),(ICAT(II),II=1,5),NCOORS,IZER
C.....JUST FOR SECOND THRU LAST FEATURES
  310 IF ( ISTAR.EQ.1 ) GO TO 320
C.......START THIS FEATURE WITH PREVIOUS LABELS RIGHT REFERENCE POINT
        XT = BOX(M-1,3)
        YT = BOX(M-1,4)
        IF ( SAVE ) WRITE (18)(ICOORS(II),II=1,4)
        IF ( NOSAVE ) CALL MOVEA ( XT, YT )
C.....START WITH RIGHT FEATURE REFERENCE TO LEFT REFERENCE POINT
  320 DO 325 I = ISTAR, ISTOP
        call spoints('X ',x,xi,i,1)
        call spoints('Y ',y,yi,i,1)
        XT = XI + XOFF
        YT = YI + YOFF
        IF ( SAVE ) WRITE ( 18 )(ICOORS(II),II=1,4)
        IF ( SAVE ) GO TO 325
          IF ( I.EQ.1 ) CALL MOVEA ( XT, YT )
          IF ( I.NE.1 ) CALL DRAWA ( XT, YT )
  325 CONTINUE
      IF ( ISTOP.EQ.NVERT ) GO TO 350
C.......LEFT LABEL REFERENCE COORDINATE
        XT = BOX(M,1)
        YT = BOX(M,2)
        IF ( SAVE ) WRITE ( 18 )(ICOORS(II),II=1,4)
        IF ( NOSAVE  ) CALL DRAWA ( XT, YT )
  350 CONTINUE
      IF ( M.LE.NBOX ) GO TO 300
C
  900 continue
C      IF ( NOSAVE ) CALL anmode
      RETURN
      END
