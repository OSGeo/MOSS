C*****************************************************************************
C
C       FILE:       S U B R O U T I N E   P L O T A
C
C       FUNCTION:   POSITION ATTRIBUTES FROM SUBJECT TABLE MAP AND/OR
C                   POSTION ASSOCIATED ATTRIBUTES FROM CHARACTERIZATION
C                   OUTPUT.  FOR THE CALCOMP PLOTTER
C
C       PARAMETERS:
C                   ICHAN      I*2     INPUT
C                   CHANNEL # MAP
C
C                   ICHAN2     I*2     INPUT
C                   CHANNEL # OF ASSOCIATED ATTRIBUTES MAP ".AT"
C
C                   POSITION ATTRIBUTES FLAG 0-NO  1-YES
C
C                   POSITION ASSOCIATED ATTRIBUTES  0-NO  1-YES
C
C                   X & Y MIN & MAX OF POLYGON
C
C          MODIFIED BY MIKE CARSELLA, TGS/BLM, MAR '86,(MULT ATT LABELS)
C          GEORGE FULLER, AUTOMETRIC INC., OCT 87 (DE-ESRI)
C          GEORGE FULLER, AUTOMETRIC INC., OCT 87 (REV UP TO FEB 87)
C
C       REQUIRED:   CALLED FROM CALCOMP.PR   COMMONS LISTED BELOW
C                   CHASH  & CHARACTER ROUTINES
C
      SUBROUTINE PLOTA(ICHAN,ICHAN2,N)
C
      CHARACTER*132 LINE
      integer mapp(2)
$Include:'xypnts.inc'
      COMMON /ATPEN/ IANP,NCLOW,NCHI
      COMMON /DIMEN/ X1,X2,Y1,Y2,XLN,YLN,SCALE,FACT,X0,Y0
      COMMON /WORKP/ ITEM(128),IBUFF(128),MAP(25),ICOM(10),
     +               JCOM(10),LAB(80)
C      COMMON /STRNG/ ICARD(80),K
$Include:'strng.inc'
      COMMON /PENS/  ILPEN,IPPEN,ISPEN,IAPEN
      COMMON /FONTHITE/  CHARH,IFONTN,SLANT
      COMMON /MDEVICE/   IDEV
      COMMON /ATTRIBUTE/ IATYP,IATYPE(10),IWORDS(10),ILOCAT(10),
     1                   IATY(10),ILABL
C      COMMON /ATTRIBUTE/ IATYP,IATYPE(10),IWORDS(10),ILOCAT(10),
C     1                   IATY(10),ILABL,ISKIP,ISECTOR(2)
      COMMON /PPLOTA/    ISKIP, ISECTOR(2)
      COMMON /CPARAM/    ICPAR(128),CPAR(64),IPAR(20,20)
      COMMON /ROTATE/    THETAD,TURN,SCAL
C
      LOGICAL SKIP,TURN
      EQUIVALENCE (YLOW,ITEM(19)),(YHIGH,ITEM(21))
      EQUIVALENCE (XCEN,ITEM(11)), (YCEN,ITEM(13))
C
      IF(NCLOW.EQ.0) RETURN
C/*  NO CHARACTERS TO PLOT
      IDTYP=ITEM(6)
      IF(IDTYP.GT.3) RETURN 
C/*  CAN'T LABEL TYPE>3 YET
C
C     RIGHT JUSTIFY STRING IF ALIGNMENT CODE IS NW OR SW.
C
      IALIG=0
      IF(ISECTOR(2).EQ.ICHAR('W')) IALIG=2
C
      CALL NEWPEN(IAPEN)
C      RANGE = YHIGH-YLOW
C      IF(RANGE.GE.10.0.AND.RANGE.LE.100.) D=15.
C      IF(RANGE.GE.101.0.AND.RANGE.LE.1000.) D=30.
C      IF(RANGE.GE.1001.0.AND.RANGE.LE.5000.) D=75.
C      IF(RANGE.GE.5001.0.AND.RANGE.LE.10000.) D=250.
C
C      IF(RANGE.GE.10001.) D=500.
C
C       LOOP THROUGH SUBJECT AND MULT ATT LABELS
C
        DO 201 II=1,ILABL
          DO 190 ICNT = 1, 25
  190       MAP(ICNT) = 0
          IF(IATY(II).NE.-1)
     1    CALL GETAT(ICHAN2,IATYPE(II),IWORDS(II),ILOCAT(II),
     2               IBUFF,ITEM(5),MAP,VALUE,IER)
          IF(NCLOW.GT.0) GO TO 200
C         USE ITEM # INSTEAD OF SUBJECT
      LINE=' '
      WRITE(LINE,1) ITEM(5)
      READ(LINE,'( 15A2)')MAP
 1        FORMAT(I5)
          L=0
          DO 205 I=1,5
            call byteit(map(i),mapp) 
            do 206 lll=1,2
            ICH=MAPP(lll)
            IF(ICH.EQ.ICHAR(' ')) GO TO 206
            L=L+1
C            CALL BYTSET(MAP,L,ICH)
 206      continue
 205      CONTINUE
          NCHAR=L
          GO TO 210
 200      CONTINUE
          IF(IATY(II).EQ.-1) CALL GETSUB(ICHAN,ITEM(24),IBUFF,MAP,ITS)
          NCHAR=NCHI-NCLOW+1
          IF(NCLOW.EQ.1) GO TO 210      
C/*  # CHARS IS FROM START
C          DO 15 I=1,NCHAR           
C/*  NOT FROM START, SHIFT CHARS
C 15         CALL BYTSET(MAP,I,MAPP(NCLOW+I-1))
 210      CONTINUE
C***********************************************************************
          GO TO (30,40,50),IDTYP
C***********************************************************************
 30       CONTINUE
          IF(II.NE.1) GO TO 60
          call spoints('X ',x,xttr,1,1)
          CALL CPTRNS(xttr,XT,X1,SCALE)    
C/*  ********** POINT ***
          call spoints('Y ',y,yttr,1,1)
          CALL CPTRNS(yttr,YT,Y1,SCALE)
          XT=XT+.1
C
C         COMPUTE NEW Y IF ALIGNMENT IS FOR SW OR SE
C
          IF(ISECTOR(1).EQ.ICHAR('S'))
     1       YT=YT-(SNGL(ILABL)*CHARH+SNGL((ILABL-1))*.33*CHARH)
C
C         COMPUTE NEW X IF ALIGNMENT IS FOR NW OR SW
C
          IF(ISECTOR(2).EQ.ICHAR('W'))
     1       XT=XT-.2
          YT=YT-CHARH
          GO TO 60
C***********************************************************************
 40       MIDDLE=N/2+1               
C/*  ********** LINE ***
          IF(N.EQ.2) MIDDLE=1
          call spoints('X ',x,xtt,middle,1)
          call spoints('Y ',y,ytt,middle,1)
c          XTT=X(MIDDLE)
c          YTT=Y(MIDDLE)
          NENT=1
          IF(MIDDLE+2.LE.N) NENT=2
          CALL CPTRNS(XTT,XT,X1,SCALE)
          CALL CPTRNS(YTT,YT,Y1,SCALE)
          call spoints('X ',x,xtt,(middle+nent),1)
          call spoints('Y ',y,ytt,(middle+nent),1)
c          XTT=X(MIDDLE+NENT)
c          YTT=Y(MIDDLE+NENT)
          CALL CPTRNS(XTT,XT2,X1,SCALE)
          CALL CPTRNS(YTT,YT2,Y1,SCALE)
          HGT=.10
          IF(FACT.LT.1.0) HGT=.1*FACT
          CALL CALLABL(HGT,MAP,NCHAR,N,XT-HGT,YT,XT2-HGT,YT2)
          RETURN
C***********************************************************************
 50       CONTINUE 
C/*  ********* POLYGON ****
          IF(II.NE.1) GO TO 60
          CALL CPTRNS(XCEN,XT,X1,SCALE) 
C/*  CONVERT CENTROID TO INCHES
          CALL CPTRNS(YCEN,YT,Y1,SCALE)
C/*  CONVERT CENTROID TO INCHES
          IF(ISECTOR(1).EQ.ICHAR('S'))
     1       YT=YT-ILABL*CHARH+(ILABL-1)*.33*CHARH
          YT=YT-CHARH
 60       CONTINUE
C
C         FOR MULTIPLE ATTRIBUTE STACKING, SPACING IS 1/3 THE HEIGHT
C
          YT=YT+.33*CHARH+CHARH
          LINES=0
          LOW=1
 65       DO 70 I=LOW,NCHAR
          call byteit(map(i),mapp)         
          IF(MAPP(1).EQ.ICHAR('\').or.mapp(2).eq.ichar('\')) GO TO 75
C/*  FOUND '\'
 70       CONTINUE
          IHI=NCHAR 
C/*  NO MORE '\'
          GO TO 80
 75       IHI=I-1
 80       NC=IHI-LOW+1
          IF(NC.EQ.0) GO TO 90
C/*  ONLY LINE FEED
          J=0
          SKIP=.TRUE.
          DO 85 I=LOW,IHI
C
C           REMOVE LEADING BLANKS
C
            call byteit(map(i),mapp)
            do 866 lll=1,2
            IF(J.EQ.0.AND.MAPP(lll).EQ.ICHAR(' ')) GO TO 86
C
            J=J+1
            lab(j)=mapp(lll)
C            CALL BYTSET(LAB,J,MAPP(1))
C
C           CHECK FOR BLANK FIELD IF SKIP OPTION SELECTED
C
            IF(ISKIP.NE.ICHAR('N').AND.LAB(i)
     +        .NE.ICHAR(' '))
     1         SKIP=.FALSE.
 866      continue
 85       CONTINUE
          YTT=YT-(CHARH+.03)*FLOAT(LINES)
          IF(.NOT.SKIP) GO TO 86
          YT=YT-.33*CHARH-CHARH
          YTT=YTT-.33*CHARH-CHARH
          GO TO 90
C
C         REMOVE TRAILING BLANKS
C
 86       CONTINUE
          I=J
 88       IF(LAB(i).NE.ICHAR(' ')) GO TO 87
          I=I-1
          IF(I.EQ.0) GO TO 87
          GO TO 88
C
C         PLOT SUBJECT OR ATTRIBUTE
C
 87       CONTINUE
          IF ( II.NE.1 ) GO TO 89
C
C         ROTATE LABEL LOCATION - CAN'T RELY ON PLOTDV BECAUSE STRING
C         WOULD BE ROTATED AS WELL.
C
          THETAR=THETAD*3.1416/180.
          XOFF=(CPAR(11))/2.
          YOFF=(CPAR(12))/2.
          XOLD=XT
          YOLD=YTT
          XNEW=(XOLD-XOFF)*COS(THETAR)-(YOLD-YOFF)*SIN(THETAR)+XOFF
          YNEW=(XOLD-XOFF)*SIN(THETAR)+(YOLD-YOFF)*COS(THETAR)+YOFF
          XT=XNEW
          YTT=YNEW
          TURN=.FALSE.
 89       IF (ISKIP.NE.ICHAR('N') .AND. SKIP ) GO TO 201
          CALL HSYMB(IDEV,IFONTN,XT,YTT,CHARH,CHARH,LAB,
     1               SLANT,0.0,0.0,IALIG,I,IER)
          TURN=.TRUE.
 90       LINES=LINES+1
          LOW=IHI+2
          IF(LOW.LE.NCHAR) GO TO 65
201     CONTINUE
 100  RETURN
      END
