C     *********************************************************
C
C              S U B R O U T I N E    H S T P L T
C
C     FUNCTION - PLOT A HISTOGRAM
C
C     INPUT PARAMETERS
C
C      FREQ   = ARRAY CONTAINING FREQUENCY VALUES
C      XTIC   = WORK AREA
C      NBARS  = NUMBER OF VALUES
C      TITLE  = ARRAY CONTAINING TITLE
C               TITLE(1) = LEN OF TITLE
C      IDEV   = OUTPUT DEVICE = 1 FOR TEK, 2 FOR CALCOMP
C
C
C     ********************************************************
C
        SUBROUTINE HSTPLT (FREQ,XTIC,NBARS,TITLE,IDEV)
        COMMON /IO/  NPRNT,IOIN
        DIMENSION  JVAL(6),FREQ(NBARS),XTIC(NBARS)
        INTEGER*2 TITLE(80)
        INTEGER*2 FONT(4)
        CHARACTER*6 CHRVAL
C
C     SET VIRTUAL WINDOW FOR SOFTWARE CHARACTER GENERATION
C
        CALL VWINDO (0.0,1.00,0.0,0.78)
        CALL SCRSET(0.0, 1.00, 0.0, 0.78)
C
C     SET OPERATING VARIABLES
C
      TIC = 10.0
      XORGIN = .01
      YORGIN = .01
      XOFF = .12
      YOFF = .05
      XRANGE = .86
      YRANGE = .65
      DO 11 I=1,6
      CHRVAL(I:I) = ' '
11    CONTINUE
C
C     FIND ODATA MINS AND MAXES TO CALCULATE HEIGHT OF BAR
C
      VALMAX = 0.0
      VALMIN = 99999.0
      DO 110 I = 1,NBARS
         VALMIN = AMIN1(FREQ(I),VALMIN)
         VALMAX = AMAX1(FREQ(I),VALMAX)
  110 CONTINUE
      RANGE = VALMAX
      HEIGHT = YRANGE/RANGE
C
C     ALL PARAMETER CALULATION FINSHED.  NOW DRAW THE
C     AXIS OF THE ACTUAL HISTOGRAM
C
      X1 = XORGIN + XOFF
      X2 = X1 + XRANGE
      Y1 = YORGIN + YOFF
      Y2 = Y1 + YRANGE
       CALL MOVEA(X1,Y1)
       CALL DRAWA(X2,Y1)
       CALL MOVEA(X1,Y2)
       CALL DRAWA(X1,Y1)
C
C     CALCULATE PARAMETERS FOR PLACING LABEL INFORMATION
C     ON THE Y AXIS
C
      TLEN = XRANGE/80.
      TVAL = 0.0
      IF ( VALMAX .GE. 20 ) GO TO 130
         YINC = YRANGE/VALMAX
         VALINT = RANGE/VALMAX
         NINT = IFIX(VALMAX) + 1
         GO TO 140
 130  CONTINUE
         YINC = YRANGE/10
         VALINT = RANGE/10
         NINT = 11
 140  CONTINUE
C
C     PLOT Y AXIS TICS
C
      YOFF1 = YOFF
      DO 150 I = 1,NINT
         X77 = XORGIN + XOFF
         Y77 = YORGIN + YOFF1
C         CALL tkplot (XORGIN + XOFF,YORGIN + YOFF1,3)
         CALL TKPLOT( X77, Y77, 3)
         X77 = X77 - TLEN
         CALL TKPLOT( X77, Y77, 2)
C         CALL tkplot (XORGIN + XOFF - TLEN,YORGIN + YOFF1,2)
         XTIC(I) = YORGIN + YOFF1 + .005
         YOFF1 = YOFF1 + YINC
  150 CONTINUE
C
C     OPEN FONTS FILE
C
      CALL plotsym(NPRNT,-61,0.0,0.0,0.0,0.0,title(2),0.0,0.0,0.0,0,
     + 5,IER)
C
C     PLOT TITLE
C
      LENTIT = TITLE(1)
      NWRD = (LENTIT-1) / 2 + 1
      XTIT = .5 - (LENTIT/2 * .0143)
      CALL plotsym (IDEV,1,XTIT,.75,.015,.015,TITLE(2),0.0,0.0,0.0,
     +0,LENTIT,IER)
c      CALL TKPLOT(XTIT,.75,3)
c      CALL TOUTPT(31)
C      CALL UNPACK(TITLE(2),NWRD)
c      DO 155 II=2,nwrd+1
C      ITMP = GTBYT(TITLE(ii),1)
c        CALL GTB( ITMP, TITLE(II), 1)
c     CALL TOUTPT(ITMP)
cC    ITMP = GTBYT(TITLE(ii),2)
c        CALL GTB( ITMP, TITLE(II), 2)
c     CALL TOUTPT(ITMP)
c155   CONTINUE
C
C     PLOT Y AXIS LABEL INFORMATION
      DO 160 I = 1,NINT
C
C  If Y values exceed integer range, JVAL is concatinated to
C   accommodate Hsymb
         call intchar(tval,jval,number)
 165     BASE = .015 * NUMBER
         CALL plotsym(IDEV,1,XORGIN,XTIC(I),.015,.015,JVAL,0.0,0.0,0.0,
     + 0,NUMBER,IER)
c         CALL TKPLOT(XORGIN,XTIC(I),3)
c      CALL TOUTPT(31)
c      DO 166 IK=1,6
c           IF(CHRVAL(IK:IK) .NE. ' ')CALL TOUTPT(ICHAR(CHRVAL(IK:IK)))
c  166    CONTINUE
         TVAL = TVAL + VALINT + .05
 160  CONTINUE
C
C     DRAW TICS ON THE X-AXIS
C
      TLEN = YRANGE * 0.025
      XINC = XRANGE/FLOAT(NBARS)
C
      XINC = XINC - XINC * 0.5
C
      XOFF1 = XOFF + XINC
      INC = 1 + (NBARS/25)
      DO 170 I = 1,NBARS,INC
        X77 = XORGIN + XOFF1
        Y77 = YORGIN + YOFF
        CALL TKPLOT( X77, Y77, 3 )
        Y77 = Y77 -TLEN
        CALL TKPLOT( X77, Y77, 2 )
C        CALL tkplot (XORGIN + XOFF1,YORGIN + YOFF,3)
C        CALL tkplot (XORGIN + XOFF1,YORGIN + YOFF - TLEN,2)
         XTIC(I) = XOFF1 + XORGIN
         XOFF1 = XOFF1 + INC * (XINC + XINC)
 170  CONTINUE
      CALL flsh
C
C     DRAW INTEGER LABELS ON X-AXIS
C
      DO 171 II=1,6
      CHRVAL(II:II)=' '
 171  CONTINUE
      XOFF1 = XOFF + XINC
C      Y = YORGIN-.015
C      XOFF1 = XOFF
      DO 180 I = 1,NBARS,INC
         TVAL = I
         CALL INTCHAR (tVAL, JVAL, NUMBER)
         BASE = .015 * NUMBER
         XPT = (XORGIN + XTIC(I)) - (BASE * .5) - .008
         CALL plotsym(IDEV,1,XPT,YORGIN,.015,.015,JVAL,0.0,0.0,0.0,
     + 0,NUMBER,IER)
 180  CONTINUE
C
C     NOW PLOT THE BARS
C
      XINC = XRANGE/(FLOAT(NBARS))
      XOFF1 = XOFF
      DO 200 I = 1,NBARS
      IF(FREQ(I).EQ.0) GO TO 190
C         HEIGHT = (YRANGE/VALMAX)/FREQ(I)
         HEIGHT = (YRANGE/VALMAX)*FREQ(I)
C         call hbar(XORGIN + XOFF1,YORGIN + YOFF,0.0,HEIGHT,XINC,0.0,1,0)
        X77 = XORGIN + XOFF1
        Y77 = YORGIN + YOFF
         call hbar(X77 , Y77 ,0.0,HEIGHT,XINC,0.0,1,0)
 190  CONTINUE
         XOFF1 = XOFF1 + XINC
 200  CONTINUE
      CALL flsh
C
C     CLOSE FONTS FILE
C
      CALL plotsym(0,-999,0.,0.,0.,0.,32,0.,0.,0.,0,0,IER)
C
C   RESET VIEWING WINDOW
C
      CALL SETWND
C
C     AND RETURN
C
      RETURN
      END
