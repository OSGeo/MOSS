C************************************************************************
C
C  Name:    L B G E N R
C
C  Purpose: THIS ROUTINE WILL LOOP THROUGH ALL OF THE FEATURES
C           AND LABEL THEM BY EITHER PRINTING THEM, OR PUTTING THEM
C           INTO A TEXT FILE (TYPE 10)
C  Calling:          AUTOLB
C******************************************************************************
C
      SUBROUTINE LBGENR ( CHAN15, PENWTH, MINS, INTER, IDD, IER )
C
       COMMON /IO/     NPRNT,IOIN
       COMMON /WORK/   IBUFF(128),NAME(25),ICOM(10),JCOM(10)
       COMMON /OSAVE/  ISUB(15),NPOLYS,LTYP2,IDISP,NOSAVE,ICTSUB,ITREC
       COMMON /LOCO1/  BOX(4,4), LENGTH, HGT, SMDIF, NSTART, NSTOP,IDO,
     +                 LOSER,NBOX,MAXBOX,IATT,IATYP,MWORDS,LOCATE,NVERT,
     +                NCHAR,LREF(4), MREF(4)
       COMMON /LOCO2/  XMIN, XMAX, YMIN, YMAX, XMID, YMID, IQUAD(4)
       COMMON /LOCO3/  NEND, ITEMS, TSCAL, ICHAN2
       COMMON /LOCO4/  ISUBJ(128), NSUBS
C
       DIMENSION   MAP(25),IWIND(8),WIND(4),ihold(15)
       INTEGER CHAN15
       CHARACTER*10 CHRBUF
       REAL        LENGTH
       LOGICAL     MCOMP, NOSAVE, LEBAL, IOK
       EQUIVALENCE ( IWIND(1), WIND(1) ),
     +             ( IBUFF( 7), WIDTH ), ( IBUFF( 9), HEIGHT ),
     +             ( IBUFF(11), ROT   ), ( IBUFF(28), PENWID ),
     +             ( IBUFF(15), XLAB  ), ( IBUFF(17), YLAB   ),
     +             ( IBUFF(19), XT2   ), ( IBUFF(21), YT2    )
C
      zero = 0.0
      izero = 0
c
C.....CHECK FOR SCREEN OUTPUT, IF NOT, THEN OPEN FILE FOR TEXT
      IFONT = 1
      MAXBOX = 4
C.....SCAN, SORT AND APPLY INTERVAL TO SUBJECT TABLE OF MAP
      CALL SBJSRT ( CHAN15, MINS, INTER, IER )
      IF ( IER.ne.0) GO TO 3010
C
C.....GET FEATURE BOUNDS
      call apget(IDD,NSTART,NSTOP,NTOTAL)
      IF(NOSAVE)call newpag
C
C.....LOOP THROUGH THE FEATURE RECORDS
      DO 1000 IDO = NSTART,NSTOP
C
C.......GET POINTER FROM FAST FILE
        CALL GETFAST(IDO,IWIND,INDEX,ISUB,IEX1,IEX2,IER)
        IF (IER .NE. 0) GO TO 3000
C.......CHECK FOR POINT IN WINDOW
        IF (INVWND(WIND(1),WIND(2),WIND(3),WIND(4),
     +             XMIN,   XMAX,   YMIN,   YMAX) .EQ. 0) GO TO 1000
C.........HOW MANY LABELS TO PLACE AND WHERE
          DO 210 IQ = 1, 4
  210       IQUAD(IQ) = 0
C.........FIRST QUADRANT?
          IF ( INVWND ( WIND(1), WIND(2), WIND(3), WIND(4),
     +                  XMID,    XMAX,    YMID,    YMAX ).EQ.1 )
     +                  IQUAD(1) = 1
C.........SECOND QUADRANT?
          IF ( INVWND ( WIND(1), WIND(2), WIND(3), WIND(4),
     +                  XMIN,    XMID,    YMID,    YMAX ).EQ.1 )
     +                  IQUAD(2) = 1
C.........THIRD QUADRANT?
          IF ( INVWND ( WIND(1), WIND(2), WIND(3), WIND(4),
     +                  XMIN,    XMID,    YMIN,    YMID ).EQ.1 )
     +                  IQUAD(3) = 1
C.........FORTH QUADRANT?
          IF ( INVWND ( WIND(1), WIND(2), WIND(3), WIND(4),
     +                  XMID,    XMAX,    YMIN,    YMID ).EQ.1 )
     +                  IQUAD(4) = 1
C.........READ FEATURE INFORMATION
          call rdblk ( CHAN15, INDEX, IBUFF, 1, IER )
          ITEM   = IBUFF(5)
          IDTYP  = IBUFF(6)
          IVAL   = IBUFF(10)
          IPOINT = IBUFF(24)
          NVERT  = IBUFF(25)
C.........READ IN FEATURE COORDINATES
          call cordxy ( IBUFF, NVERT, INDEX, WIND(1), WIND(3), CHAN15 )
          CALL CLEAR  ( ISUB, 15 )
C
          IF ( IATT.EQ.0 ) CALL GETSUB ( CHAN15,IPOINT,IBUFF,ISUB,II )
c          IF ( IATT.NE.0 ) CALL GETAT ( ICH26, IATT, MWORDS, LOCATE,
c     +                                  IBUFF, ITEM, ISUB, RNUM, IER )
C
          nchars=15
          CALL LASTC ( ISUB, NCHARS )
          CALL NBCHRS ( ISUB, NCHARS,IFRST,ILAST,IER )
          NCHAR = ILAST - IFRST + 1
          LENGTH = FLOAT ( NCHAR ) * HGT
C.........DECODE SUBJECT FOR NUMERIC COMPARISON
          IF ( NCHAR.LE.5 ) then
         kount=0
         do 230 jj=IFRST,ILAST
           kount = kount + 1
             CALL GOFF( INXX, JJ, ISUB(1) )
           chrbuf(kount:kount) = char(INXX)
  230         continue
            READ(chrbuf(1:5),245,IOSTAT=IER,ERR=3020) ITSUB
  245       FORMAT(I5)
       ENDIF
C.........CHECK IF THIS SUBJECT/ATTRIBUTE IS TO BE LABELED
          LEBAL = .FALSE.
          DO 250 II = 1, NSUBS
  250       IF ( ABS(ITSUB).EQ.ABS(ISUBJ(II)) ) LEBAL = .TRUE.
C.........DETERMINE COORDINATES OF LABEL POSITION(S)
          NV = NVERT
          CALL LBLBOX ( LEBAL,NV, IER )
       IF(IER .NE. 0) GO TO 3040
C
          IF (NBOX .LT. 1) GO TO 400
C...........DETERMINATE FIRST AND LAST PRINTABLE CHARACTERS
c            CALL FRSTC ( IOK, ISUB, 1, 15, ION )
c            IF ( ION.EQ.1 ) GO TO 325
c              nchars=15
c              CALL LASTC ( ISUB, NCHARS )
C  force right justified elev. back into the subject
             do 251 il=1,5
251         ihold(il)=ichar(chrbuf(il:il))
            call packc(ihold,isub,15)
             N = 0
C
C...........PLACE LABEL
  325       DO 375 II = 1, NBOX
              XT1 = BOX(II,1)
              YT1 = BOX(II,2)
              XT2 = BOX(II,3)
              YT2 = BOX(II,4)
C.............CALCULATE LABEL POSITION AND ANGLE OF ROTation
              CALL CALCLB ( NCHAR, HGT, ROT,
     +                      XT1, YT1, XT2, YT2, XLAB, YLAB )
              IF (NOSAVE )
     +           call plotsym(0,IFONT,XLAB,YLAB,
     +             HGT,HGT,ISUB,ROT,zero,rot,izero,NCHAR,IER)
              IF ( NOSAVE ) GO TO 375
C...............YES - HANDLE TEXT MAP ENTRIES FOR LABEL(S) - DIRECT ASSIGNMENTS
                IBUFF(1)  = NEND + 1
                IBUFF(2)  = 1
                IBUFF(3)  = NEND + 1
                IBUFF(4)  = 0
                IBUFF(5)  = ITEMS + 1
                IBUFF(6)  = 10
                IBUFF(13) = 0
                IBUFF(14) = 0
                IBUFF(23) = 0
                IBUFF(25) = NCHAR
                IBUFF(26) = IFONT
                DO 330 J = 27, 39
  330             IBUFF(J) = 0
C...............MOVELABEL INTO TEXT MAP BUFFER
                DO 340 J = 1, (NCHAR+1)/2
  340             IBUFF(39+J) = ISUB(J)
C...............PAD LABEL STORAGE SPACE AND RESERVED SPACE WITH NULLS
                DO 350 J = ((NCHAR+1)/2)+40, 128
  350             IBUFF(J) = 0
C...............ASSIGNMENTS THRU EQUIVALENTS
                WIDTH  = HGT
                HEIGHT = HGT
                PENWID = PENWTH
                CALL WRBLK ( ICHAN2, NEND, IBUFF, 1, IER )
          IF(IER .NE. 0) GO TO 3060
                NEND  = NEND + 1
                ITEMS = ITEMS + 1
  375       CONTINUE
  400     CONTINUE
 1000 CONTINUE
      GO TO 9000
C
C.....ERROR RETURNS
C
 3000 WRITE(NPRNT,3001) IER, I, INDEX
 3001 FORMAT(' ERROR ',I5,' FROM GETFAST INDEX =',2I7,'  *LBGENR*')
      GO TO 9000
C
 3010 WRITE(NPRNT,3011) MINS, IER
 3011 FORMAT(' NON-NUMERIC SUBJECT RECORD =',I6,' ERROR ',I5,
     +       '  *LBGENR*')
      GO TO 9000
C
 3020 WRITE(NPRNT,3021) ISUB
 3021 FORMAT(' DECODE ERROR ON SUBJECT ',15A2,'  *LBGENR*')
      GO TO 9000
C
 3040 WRITE(NPRNT,3041) IER
 3041 FORMAT(' ERROR ',I5,' FROM LBLBOX   *LBGENR*')
      GO TO 9000
C
 3060 WRITE(NPRNT,3061) IER
 3061 FORMAT(' ERROR ',I5,' FROM WRBLK   *LBGENR*')
C
 9000 RETURN
      END
