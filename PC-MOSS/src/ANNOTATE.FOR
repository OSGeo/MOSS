C************************************************************************
C
C        P R O G R A M    A N N O T A T E
C
C  FUNCTION-This is the logical driver for the ANNOTATE command
C          This routine is executed to place a text message on
C          A PLOT.  The plot command has been executed prior to
C          executing the annotate command, and a theme or map
C          is present on the screen, therefore the screen must not
C          be erased, since it is being used to position the text.
C
C      This routine will all the user to add data to existing
C      ANNOTATE files and to create new ANNOTATE files.
C
C*************************************************************************
C
      PROGRAM ANNOTATE
C
      CHARACTER*80     ALF
      CHARACTER*3      ANS
      CHARACTER*1      ABYTE, IAN
$include:'strng.inc'
c      COMMON /STRNG/   ICARD(80),K
$include:'work.inc'
c      COMMON /WORK/    IBUFF(128),MAP(15),ICOM(10),JCOM(10)
      COMMON /USER/    NAMES(10), IPASS(10)
      COMMON /MAPLEG/  SCALE,RANGE
      COMMON /MDEVICE/ IDEV
      COMMON /FLS/     IUNIT,IFILE(5)
      DIMENSION IALF(40),kfontf(4)
      EXTERNAL ABYTE, BYTE
      EQUIVALENCE(IALF,ALF)
      DATA SMVAL / .00001 /
      data kfontf/2HFO,2HNT,2H.D ,2HAT/
C
C      INITIALIZE THE PARAMETERS.
C
      CALL initl(ier)
      if(ier.ne.0)stop
C
20    CONTINUE
C
C
C initilize display
C
      map(1)=2HLM
      map(2)=2h
      jchan=81
      call openf(jchan,map,1,0,1,ier)
      if(ier.ne.0)then
        write(nprnt,2)
   2  format(/,1x,'You must do the <PLOT> command first')
        return
      endif
C
      map(1)=2hZ
      call openf(70,map,3,0,1,ier)
C
      call initt(1,iunit,ifile)
C
C       OPEN HERSHEY FONTS FILE
C
        nnchar=4
        CALL plotsym(22,-61,0.,0.,0.,0.,Kfontf,0.,0.,0.,0,nnchar,IER)
C
C   SET WINDOW FOR TEKTRONIX PLOTTING
C
      CALL WHWND(X1,Y1,X2,Y2)
      XRAN=X2-X1
      YRAN=Y2-Y1
      IF(XRAN.GE.YRAN) SCAL=XRAN/10.24
      IF(XRAN.LT.YRAN) SCAL=YRAN/7.88
      SCAL=SCAL/2.
      CALL SETWND
C
C      GET THE NAME OF THE ANNOTATE FILE.
C
22      CONTINUE
c      CALL CPLAC(1)
c      CALL ANMODE
      WRITE(*,1060)
 1060 FORMAT(/' ENTER the name of the ANNOTATE file:',\)
      call readin(map,25,is,ie)
C
C      CALL CPLAC(3)
C      CALL ANMODE
C
C.....OPEN THE FILE OLD... IF AN ERROR, THEN OPEN FRESH
C
      CALL OPENF(21,map,1,0,0,IER)
      if(ier.ne.0)then
       write(*,*)
       write(*,*)' A new Annotate file is being created'
       call openf(21,map,0,0,0,ier)
       iold=1
       go to 50            
      else 
C
C.....AN ANNOTATE FILE WITH THE SAME NAME ENTERED
C.....ALL READY EXISTS, INFORM THE USER AND SEE
C.....IF IT IS TO BE DISPLAYED ON THE SCREEN.
C
24    CONTINUE
      IOLD = 1
      items=0
      NCHFILE=NCHFILE-1
      WRITE(*,1070) (map(l),l=1,12)
1070  FORMAT(/' The ANNOTATE file named: ',12A1,' already exists.'/)
      WRITE(*,1000)
1000  FORMAT(' Do you want to see the ANNOTATE file ',
     +       ' (Y)/N'\)
      call readin(iget,1,is,ic)
      IF (iget.eq.ichar('N')) GO TO 50
      write(*,*)
      WRITE(NPRNT,1013)
 1013 FORMAT(//,' MAP WILL RE-DRAW - then do the following:',
     +     /,' ** After viewing the annotate text - ',
     +     /,' ** Hit a CR to continue.',/)
      write(*,44)
  44   format(/,1x,'< CR > to proceed '\)
      call readin(iget,1,ic,iend)
C
C.....DISPLAY PREVIOUS ANNOTATE FILE
C
      rewind (jchan)
C erase first
      write(*,*)char(27),char(12)
      call drfast(jchan,ier)
C
25    READ(21,1020,END=51) (IALF(I),I=1,40)
1020  FORMAT(40A2)
      READ(21,1030,ERR=51,END=51) XP,YP,ICOLOR,
     +            HITE,ANGLE,IFONT,WIDTH
1030  FORMAT(2F10.1,I5,2F10.3,I5,F10.3)
      IF ( ABS(WIDTH).LT.SMVAL ) WIDTH = HITE
      IF(IFONT.EQ.0) IFONT=12
      DO 26 NCH=80,1,-1
C/*  COUNT # CHARACTERS IN TEXT
         IF(alf(nch:nch).NE.' ' ) GO TO 28
 26   CONTINUE
 28   CALL plotsym(IDEV,IFONT,XP,YP,WIDTH*SCAL,HITE*SCAL,IALF,
     +0.0,0.0,ANGLE,0,NCH,IERR)
      GO TO 25
C
51    call flsh

      read(*,'(a1)') ans
      write(*,*)char(27),char(12)
      backspace(21)
       iold=0
       items=2
      endif 
C
50    CONTINUE
C
C.....IF THIS IS A NEW FILE GO ADD TEXT (IOLD .EQ. 1)
C
      IF(IOLD .EQ. 1) GO TO 52
C
C.....SEE IF USER WANTS TO ADD DATA TO THE EXISTING FILE.
C
      WRITE(*,2010)(map(l),l=1,12)
2010  FORMAT(/' Do you want to add more TEXT to the file named: ',
     +12a1,' (Y)/N'\)
      call readin(iget,1,ic,is)
      IF (iget.eq.ichar('N')) GO TO 100
C
52    CONTINUE
       if(iold.eq.1)then
        do 54 il=1,5000
54      read(21,'(a1)',end=56) ans
56      backspace(21)
        IOLD = 0
       endif
C
C     BEGIN THE LOOP TO ADD LINES OF TEXT TO THE ANNOTATE FILE.
C     FIRST GET THE CHARACTER SIZE, ANGLE, AND FONT.
C
60    CONTINUE
      IERR=0
      WRITE(*,1050)
1050  FORMAT(/' Enter the following:',/,' Font Number, Pen Number, ',
     +'Char. Height, Char. Width, Text Angle ?'\)
      READ(*,*,ERR=61,END=60)IFONT,ICOLOR,HITE,WIDTH,ANGLE
      IF(IFONT.LT.0) GO TO 60
C/*  BAD FONT
      IF(IFONT.EQ.0) IFONT=12
      IF(ICOLOR.LT.1 .OR. ICOLOR.GT.16)go to 61
C/*  BAD PEN #
      IF(HITE.LE.0. .OR. HITE.GT.1.5) GO TO 61
C/*  BAD HITE
      IF(WIDTH .LE.0. .OR. WIDTH .GT.1.5) GO TO 61
C/*  BAD width
      go to 65
61    write(*,*)'Incorrect value entered - try again'
      go to 60
C
C.....ACCEPT A LINE OF TEXT FROM THE USER.
C
65    write(*,*)char(27),char(12)
      WRITE(*,1040)
1040  FORMAT(' ENTER a line of TEXT (up to 80 char.) - CR to quit.')
      write(*,'(1x,2h? ,\)')
      read(*,'(a)')alf
      iold=0
      IF (alf(1:1).eq.' ') GO TO 50
C
C.....GET THE COORDINATES FOR BEGINING OF THIS LINE OF TEXT.
C
      WRITE(NPRNT,1012)
 1012 FORMAT(//,' MAP WILL RE-DRAW - then do the following:',
     +     /,' ** Use cursor keys to position the crosshairs - ',
     +     /,' ** Hit SPACE BAR to mark the position for the text -'
     +     /,' ** Hit any other key to QUIT',
     +     /,' ** After viewing results - CR to continue.',/)
      write(*,4)
  4   format(//,1x,'< CR > to proceed or < Q > to quit'\)
      call readin(iget,1,ic,iend)
      if(iget.eq.ichar('Q'))go to 100
C
C  GET POINT
C
      rewind (jchan)
C erase first
      write(*,*)char(27),char(12)
      call drfast(jchan,ier)
      if(items.gt.1) then
       rewind(70)
       call drfast(70,ier)
       backspace(70)
      endif
      CALL VCURSR(IH,XP,YP)
C
C.....DISPLAY THE LINE OF TEXT ON THE SCREEN.
C
CGDM      NCH=NCH-1
      DO 27 NCH=80,1,-1
C/*  COUNT # CHARACTERS IN TEXT
         IF(alf(nch:nch).NE.' ' ) GO TO 29
 27   CONTINUE
 29   CALL plotsym(IDEV,IFONT,XP,YP,WIDTH*SCAL,HITE*SCAL,IALF,
     +0.0,0.0,ANGLE,0,NCH,IERR)
      IF(IERR.NE.0) GOTO 60
C
C      ADD THE LINE OF TEXT ALONG WITH COORD., FONT, ETC.
C      TO THE ANNOTATE FILE.
C
      call flah
      read(*,'(a1)') ans
C
      DO 70 I=NCH+1,40
         ialf(i)=8224
70    CONTINUE
      WRITE(21,1020) (IALF(I),I=1,40)
      WRITE(21,1030) XP,YP,ICOLOR,HITE,ANGLE,IFONT,WIDTH
      GO TO 65
C
C.....ANNOTATE FILE COMPLETE, SO CLOSE IT AND SEE IF THE USER
C.....WANTS TO CREATE ANOTHER FILE.
C
100   CLOSE ( 21 )
C
      WRITE(*,1090)
1090  FORMAT(' Do you want to create another ANNOTATE file',
     1       ' Y/(N)'\)
      call readin(iget,1,is,ic)
      IF(iget.eq.ichar('Y'))go to 22
C
      GO TO 90000
C
9000  CONTINUE
C
90000 CONTINUE
      END
