C******************************************************************************
C
C     NAME:         SUBROUTINE HSYPOS
C
C     FUNCTION:    FINDS NEW X AND Y FOR STARTING POSITION OF THE TEXT STRING
C                  WHEN ALIGMENT CODE IS OTHER THAN 0.
C
C     PARAMETERS:
C
C                   *      I*2      INPUT
C                   THE CHANNEL TO WRITE OUTPUT TO.
C
C                   NFONT     I*2      INPUT
C                   THE FONT NUMBER TO SELECT AND THE INITIALIZATION & STOP.
C
C                   XLOC      R*4      INPUT
C                   THE X(ORDINATE) LOCATION TO BEGIN PLOTTING.
C                   IF XLOC = 999. THEN PLOTTING CONTINUES FROM LAST Y LOCATION
C
C                   YLOC      R*4      INPUT
C                   THE Y(ABSISSA)  LOCATION TO BEGIN PLOTTING.
C                   IF YLOC = 999. THEN PLOTTING CONTINUES FROM LAST Y LOCATION
C
C                   XHGT      R*4      INPUT
C                   THE X HEIGHT OF THE CHARACTER(S)
C
C                   YHGT      R*4      INPUT
C                   THE Y HEIGHT OF THE CHARACTER(S)
C
C                   ITEXT     I*2      INPUT
C                   THE TEXT IN PACKED FORMAT(MAX # OF CHARS A WORD) TO BE PLOTTED.
C
C                   ANGL      R*4      INPUT
C                   ANGLE IN DEGREES FROM THE X-AXIS AT WHICH THE CHARACTER(S)
C                   IS TO BE PLOTTED.  IF ANG = 0.0  THE CHARACTERS ARE
C                   PLOTTED RIGHT SIDE UP AND PARRELLEL TO THE X-AXIS.
C
C                   EXP       R*4       INPUT
C                   EXPANSION FACTOR (SPACE INBETWEEN CHARACTERS).
C
C                   ROTATE    R*4     INPUT
C                   ANGLE IN DEGREES FROM THE X-AXIS AT WHICH THE INDIVIDUAL
C                   CHARACTER(S) ARE TO BE PLOTTED.
C
C                   IALIG     I*2      INPUT
C                   ALIGMENT CODE NUMBER. THIS ROUTINE IS ONLY USED IF IALIG
C                   IS OTHER THAN 0. IALIG'S RANGE IS 0-8.
C                   0-LOWER LEFT    3-CENTER LEFT      6-UPPER LEFT
C                   1-LOWER CENTER  4-CENTER CENTER    7-UPPER CENTER
C                   2-LOWER RIGHT   5-CENTER RIGHT     8-UPPER RIGHT
C
C                   NCHAR     I*2      INPUT
C                   NUMBER OF CHARACTERS TO BE PLOTTED.
C
C                   XNEW      R*4      OUTPUT
C                   THE NEW X POSITION
C
C                   YNEW      R*4      OUTPUT
C                   THE NEW Y POSITION
C
C                   WIND       (R*4)    OUTPUT
C                   THE ARRAY THAT CONTAINS XMIN,XMAX,YMIN,YMAX
C
C                   IER       I*2      OUTPUT
C                   THE RETURNED ERROR PARAMETER SET TO ONE NO ERRORS
C                   IER = -50  ILLEGAL OPEN
C                   IER = -55  ILLEGAL ALIGMENT CODE
C                   IER = -56  ILLEGAL FONT
C
C
C     PROGRAMMER:   ANNE TAYLOR (AUTOMETRIC) JUNE-1985
C                   (REVISED FROM HSYMB ROUTINE)
C
C     REQUIRED:     COMMONS:  HHEAD,HFONT,HCUR
C                   ROUTINES: BYTE,DSIN
C                   FILES:    A FONT FILE WITH LENGTH 'KLEN'
C
C     REFERENCES:   A CONTRIBUTION TO COMPUTER TYPESETTING TECHNIQUES:
C                   TABLES OF COORDINATES FOR HERSHEY'S REPERTORY OF
C                   OCCIDENTAL TYPE FONTS AD GRAPHIC SYMBOLS,
C                   NORMAN M WOLCOTT & JOSEPH HILSENRATH,
C                   U.S. DEPARTMENT OF COMMERCE/NATIONAL BUREAU OF STANDARDS,
C                   NBS SPECIAL PUBLICATION 424, APRIL 1976.
C
C
C*****************************************************************************
       SUBROUTINE HSYPOS(IOUT,NFONT,XLOC,YLOC,XHGT,YHGT,ITEXT,ROTATE,
     +      EXP,ANGL,IALIG,NCHAR,XNEW,YNEW,WIND,IER)
C
C**     MACHINE DEPENDENT FEATURES.  SET FOR 16 BIT 2 BYTE WORDS
C**     SET RECORD LENGTH,BYTES PER WORD,ARRAY DIMENSIONS
C**     KDIM NEEDS TO BE SET TO THE RECORD LENGTH DIVIDED BY THE BYTES PER WORD
C**     KFONTS IS THE MAX ALLOWABLE NUMBER OF FONTS & CHARACTERS IN EACH FONT
C
       PARAMETER (KLEN = 256, KBYTE = 2, KDIM = 128, KFONTS = 128)
C
       COMMON /HHEAD/ IHEAD(128)
       COMMON /HFONT/ IFONT(128),XFO,YFO,XFSCL,YFSCL,IFRST
       COMMON /HCURR/ ICHAN,IFNUM,IRCUR,ICBUF(128),XL,YL
C
       DIMENSION ITEXT(1),WIND(4),ITRAN(8),ihold(2),itex(160)
C       INTEGER BYTE
       EQUIVALENCE (ITRAN(1),XFO)
       DIMENSION XY36(2),XY147(2),XY258(2),XY12(2),XY345(2),XY678(2)
       DATA SMVAL / .00001 /
C
       XL1 = XLOC
       YL1 = YLOC
       IF(XLOC .EQ. 999.)XL1 = XL
       IF(YLOC .EQ. 999.)YL1 = YL
C
C** CHECK TO MAKE SURE IALIG IS IN RIGHT RANGE
C
       IF( (IALIG.LT.0) .OR. (IALIG.GT.8) ) GO TO 85020
C
C**  SAME FONT OR DIFFERENT ONE ?...IF DIFF READ HEADER & SET CURRENT FONT
C
       IF(IFNUM .EQ. -32767) GO TO 85000
       IF (NFONT .EQ. IFNUM) GO TO 1200
       IF (NFONT .LT. 0  .OR.  NFONT .GT. 127) GO TO 85060
       IRCUR = IHEAD(NFONT)
       IF (IRCUR .EQ. 0) GO TO 85060
       CALL RDBLK(ICHAN,IRCUR,ICBUF,1,IER)
       IF (IER .NE. 0) GO TO 85040
       IFNUM = NFONT
C
C**     LOAD THE HEADER FOR THE NEWLY SELECTED FONT INTO COMMON
C**     1 -      RECORD # OF FIRST COORD REC          2 - 3  X OFFSET
C**     3 - 4    Y OFFSET   5 - 6      X MAPPED ON    7 - 8  Y MAPPED ON
C**     9 - 127  RESERVED   128 - 256  FIRST WORD OF CHAR...ZERO IF NOT THERE
C
         IFRST = ICBUF(1)
         DO 1020 I = 1,8
           ITRAN(I) = ICBUF(I+2)
1020     CONTINUE
         ircur=ircur+1
         call rdblk(ichan,ircur,icbuf,1,ier) 
         DO 1040 I = 1,KFONTS
           IFONT(I) = ICBUF(I)
1040     CONTINUE

C
C**  MOVE INPUT VALUES TO PARAMETERS
C
1200   CONTINUE
       ANG = ANGL
       ANG = MOD(ANG,360)
       XLL = XL1
       YLL = YL1
       IF ( ABS(XFSCL).LT.SMVAL ) THEN
          XH = 0.
       ELSE
          XH = XHGT / XFSCL
       END IF
       IF ( ABS(YFSCL).LT.SMVAL ) THEN
          YH = 0.
       ELSE
          YH = YHGT / YFSCL
       END IF
       COSA = COS(ANG * .01745329)
       SINA = SIN(ANG * .01745329)
       IF((ANG.NE.90.) .AND. (ANG.NE.270.)) TANA = TAN(ANG * .01745329)
       SINR = SIN(ROTATE * .01745329)
       IKNT = NCHAR
       ICENT = 0
       XEXP = EXP * COSA
       YEXP = EXP * SINA
C
C**     M A I N  L O O P
C** FINDS LENGTH OF CHARACTER STRING...(XL1,YL1) IS THE FIRST POINT OF THE STRING
C** AND (XLL,YLL) WILL BE THE LAST POINT OF THE STRING
C
2000   CONTINUE
       do 2001 ll=1,nchar/2+1
2001   itex(ll)=itext(ll)
       call unpack(itex,nchar)
       DO 8020 IK = 1,IKNT
         ICHAR9 = ITEX(IK)
         ISTRT = IFONT(ICHAR9+1)
C
C**     CHARACTER SPECIFIED NOT IN THE FONT ?
C
         IF (ISTRT .NE. 0) GO TO 2200
         XLL = XLL + (XHGT * COSA)
         YLL = YLL + (YHGT * SINA)
         GO TO 8000
C
C**     DIFFERENT RECORD FOR COORDINATES OR THE SAME ONE ?
C
2200     CONTINUE
         IIRCUR = IABS(ISTRT)/KDIM + IFRST
         IF (IIRCUR .EQ. IRCUR) GO TO 2400
         IRCUR = IIRCUR
         CALL RDBLK(ICHAN,IRCUR,ICBUF,KDIM,IER)
         IF (IER .NE. 0) GO TO 85040
C
C**     GET START & STOP FOR THIS CHARACTER
C
2400     CONTINUE
         ICBKNT = MOD(IABS(ISTRT),KDIM)
         NTPTS  = IABS(ICBUF(ICBKNT))
         ICBKNT = ICBKNT + 1
         call byteit(icbuf(icbknt),ihold)
         XPOS = ISIGN(ihold(1),ISTRT)
         YPOS = ISIGN(ihold(2),icbuf(icbknt))
         IF (ICENT .NE. 1) XPOS = 0.0
         IF (ICENT .NE. 1) YPOS = 0.0
         NPTS = 0
C
C**     EXTRACT COORDINATES LOOP
C
3000       CONTINUE
           NPTS = NPTS + 1
           IF (NPTS .GT. NTPTS) GO TO 6000
           ICBKNT = ICBKNT + 1
           IF(ICBKNT .LE. 128) GO TO 3600
           ICBKNT = 1
           IRCUR = IRCUR + 1
           CALL RDBLK(ICHAN,IRCUR,ICBUF,1,IER)
           IF (IER .NE. 0) GO TO 85040
C
C**     GET COORDINATES THEN OFFSET,SCALE,PATH,ROTATE AND PUT TO DEVICE
C
3600         CONTINUE
             call byteit(icbuf(icbknt),ihold)
             IX = ihold(1)
             IY = ihold(2)
             IF (IX .EQ. 0  .AND.  IY .EQ. 0) GO TO 3000
C**  OFFSET,SCALE
               X1 = ( (IABS(IX) - XFO - XPOS) * XH)
               Y1 = ( (     IY  - YFO - YPOS) * YH)
C**  PATH
               XP = ( X1 * COSA - Y1 * SINA ) + XLL
               YP = ( X1 * SINA + Y1 * COSA ) + YLL
C**  ROTATE
               XP = XP - ( Y1 * SINR )
               IF((ANG .EQ. 90.0 ).OR.(ANG .EQ. 270.0))
     +             YP = YP - ( Y1 *SINR )
           GO TO 3000
C
C**     MOVE TO BEGINNING OF NEXT CHARACTER
C
6000   CONTINUE
       XLL = XLL + (XP-XLL) + XEXP
       YLL = YLL + (YP-YLL) + YEXP
       X1 = XLL
       Y1 = YLL
8000   CONTINUE
8020   CONTINUE
C
C**  COMPUTE LAST POINT OF CHARACTER STRING
C
       XLL = XLL - (XP-XLL) - XEXP
       YLL = YLL - (YP-YLL) - YEXP
C
C**  SET NEW X,Y TO XL1,YL1 ; XLAST,YLAST TO XLL,YLL; AND BRANCH IF IALIG = 0;
C**   (DON'T NEED TO COMPUTE NEW X,Y POSITION)
C
       XNEW = XL1
       YNEW = YL1
       XLAST = XLL
       YLAST = YLL
       IF(IALIG .EQ. 0)GO TO 9275
C
C**     LOOP TO FIND NEW X,Y POSITION AND TO FIND THE NEW X,Y LAST POSITION
C
      XLEN = (XLL - XL1)
      YLEN = (YLL - YL1)
       DO 9250 I=1,2
         IF(I .EQ. 2) XL1 = XLL
         IF(I .EQ. 2) YL1 = YLL
C
C**     COMPUTE NEW POSITION: FIRST, FORMULAS FOR X AND Y ARE SET TO A CONSTANT
C**     THE NUMBERS REFER TO THE ALIGMENT CODE #, AND X IS ALWAYS IN THE 1ST
C**     POS OF THE XY### ARRAY AND Y IS ALWAYS IN THE 2ND POS OF THE ARRAY
C
C**   COLUMNS
         XY36(1) = XL1
         XY36(2) = YL1
         XY147(1) = XL1 - .5 * XLEN
         XY147(2) = YL1 - .5 * YLEN
         IF((ANG .NE. 90.).AND.(ANG .NE. 270.))
     +       XY147(2) = TANA * (.5 * XLEN)
         XY258(1) = XL1 - XLEN
         XY258(2) = YL1 - YLEN
         IF((ANG .NE. 90.) .AND. (ANG .NE. 270.))XY258(2) = TANA * XLEN
C**   ROWS
         XY12(1) = XL1
         XY12(2) = YL1
         XY345(1) = XL1 + .5 * XHGT
         XY345(2) = YL1 - .5 * YHGT
         XY678(1) = XL1 + XHGT
         XY678(2) = YL1 - YHGT
C
C**  NEW Y'S IF ANG NE 90 OR 270, AND NEW X'S IF ANG = 90 OR 270
C
         IF( (IALIG .EQ. 1) .OR. (IALIG .EQ. 2) )GO TO 9000
         IF((IALIG.EQ.3) .OR. (IALIG.EQ.4) .OR. (IALIG.EQ.5))GO TO 9020
         IF((IALIG.EQ.6) .OR. (IALIG.EQ.7) .OR. (IALIG.EQ.8))GO TO 9040
C
9000     CONTINUE
         IF( (ANG.NE.90.) .AND. (ANG.NE.270.) ) YNEW1 = XY12(2)
         IF( (ANG.EQ.90.) .OR. (ANG.EQ.270.) ) XNEW1 = XY12(1)
         GO TO 9080
C
9020     CONTINUE
         IF( (ANG.NE.90.) .AND. (ANG.NE.270.) ) YNEW1 = XY345(2)
         IF( (ANG.EQ.90.) .OR. (ANG.EQ.270.) ) XNEW1 = XY345(1)
         GO TO 9080
C
9040     CONTINUE
         IF( (ANG.NE.90.) .AND. (ANG.NE.270.) ) YNEW1 = XY678(2)
         IF( (ANG.EQ.90.) .OR.(ANG.EQ.270.) ) XNEW1 = XY678(1)
         GO TO 9080
C
C**     NEW X'S & Y'S IF ANG NE 90 OR 270  AND NEW Y'S IF ANG = 90 OR 270
C
9080     CONTINUE
         IF( (IALIG.EQ.3) .OR. (IALIG.EQ.6) ) GO TO 9100
         IF((IALIG.EQ.1) .OR. (IALIG.EQ.4) .OR. (IALIG.EQ.7)) GO TO 9120
         IF((IALIG.EQ.2) .OR. (IALIG.EQ.5) .OR. (IALIG.EQ.8)) GO TO 9140
C
9100     CONTINUE
         IF( (ANG.NE.90.) .AND. (ANG.NE.270.) )XNEW1 = XY36(1)
         IF( (ANG.EQ.90.) .OR. (ANG.EQ.270.) )YNEW1 = XY36(2)
         GO TO 9200
C
9120     CONTINUE
         IF( (ANG.NE.90.) .AND. (ANG.NE.270.) )XNEW1 = XY147(1)
         IF( (ANG.NE.90.) .AND. (ANG.NE.270.) )YNEW1 = YNEW1 - XY147(2)
         IF( (ANG.EQ.90.) .OR. (ANG.EQ.270.) )YNEW1 = XY147(2)
         GO TO 9200
C
C
9140     CONTINUE
         IF( (ANG.NE.90.) .AND. (ANG.NE.270.) )XNEW1 = XY258(1)
         IF( (ANG.NE.90.) .AND. (ANG.NE.270.) )YNEW1 = YNEW1 - XY258(2)
         IF( (ANG.EQ.90.) .OR. (ANG.EQ.270.) )YNEW1 = XY258(2)
         GO TO 9200
C
9200     CONTINUE
         IF(I .EQ. 1) XNEW = XNEW1
         IF(I .EQ. 1) YNEW = YNEW1
         IF(I .EQ. 2) XLAST = XNEW1
         IF(I .EQ. 2) YLAST = YNEW1
C
9250   CONTINUE
C
C**  PUT MIN AND MAX X'S AND Y'S INTO WIND (XMIN,XMAX,YMIN,YMAX)
C
9275   CONTINUE
       WIND(1) = XNEW
       WIND(2) = XLAST
       WIND(3) = YNEW
       WIND(4) = YLAST + YHGT
C
       GO TO 90000
C
C**     ERROR RETURNS
C
85000  CONTINUE
       IER = -54
       WRITE(*,98500)NFONT,IER
98500  FORMAT(' *HSYPOS*   FONT ',I5,' HAS NOT BEEN INITIALIZED ',
     +        '  ERROR ',I5)
       GO TO 90000
85020  CONTINUE
       IER = -55
       WRITE(*,98502)IALIG,IER
98502  FORMAT(' *HSYPOS*  ALIGMENT CODE ',I4,' ILLEGAL   ERROR ',I5)
       GO TO 90000
85040  CONTINUE
       WRITE(*,98504) IREC,IER
98504  FORMAT(' *HSYPOS*  ERROR FROM IN OF REC #',I5,' ERROR',I5)
       GO TO 90000
85060  CONTINUE
       IER = -56
       WRITE(*,98506)NFONT,IER
98506  FORMAT(' *HSYPOS* FONT NUMBER ',I5,' IS ILLEGAL  ERROR ',I5)
       GO TO 90000
C
C**     JUMP OUT
C
90000  CONTINUE
       RETURN
       END
