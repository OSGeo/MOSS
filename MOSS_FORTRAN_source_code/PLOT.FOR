C******************************************************************************
C
C             M A I N    P L O T
C
C FUNCTION:   PLOT THE CONTENTS OF THE ACTIVE FILE (NDAT)
C
C PARAMETERS:
C             IER       IO  - THE ERROR RETURN FOR WRITING TO, READING,
C                             OPENNING, OR CLOSING A FILE,
C                             IER = 1, IF AN ERROR,
C                             IER = 0, IF NO ERROR.
C ROUTINES
C
C******************************************************************************
C
$Include:'DBCHAN.inc'
$Include:'DNAMES.inc'
$Include:'IO.inc'
$Include:'MFAST.inc'
$Include:'STRNG.inc'
$Include:'WHMAP.inc'
       COMMON /HCURR/  ICHANH,IFNUM,IRCUR,ICBUF(256),X,Y
       COMMON /FONTY/  KFONT,XORG,YORG,XINC,YINC,FSCAL,FLOOD
       COMMON /PLACT/  MACTS
       COMMON /TABLE/  ITABLE(40)
       COMMON /TYPE/   IDATYP(40), NACTS
C       COMMON /RBX/    IRBGX,ICHX
C       COMMON /RBY/    IRBGY,ICHY
       COMMON /COLOR/ ICOLOR,KCOLOR(40)
       COMMON /FLS/    IUNIT,IFILE(5)
       COMMON /CLINES/ ICLINE, IBATCH
       COMMON /LOCAL/  ICHAN,ISUB,JERE,WIND(4),IX1,IX2,IY1,IY2,
     +                 IPOT1,ISTOP,ICONV,KNEW,NTOTAL,INDEX,IEX1,IEX2,
     +                 XXLEN,YYLEN,XR,YR,XOFF,YOFF,TOTPIX,BIGAREA,IDD,IDDT
       DIMENSION IDISP(10), MAP(25),jcard(80),NCARD(80)
       REAL      IX1,IY1,IX2,IY2,DINC
       LOGICAL   BORD,FAST,FLOOD,ABORT,LTIME,REPEAT,unityes,
     +           COLOR,SCOLOR,MCOMP,TICS
C
      FLOOD = .FALSE.
C
      CALL Initl(ier)
C
C      IF ( ICARD(80).EQ.-1 ) FLOOD  = .TRUE.
C      IF ( ICARD(78).EQ.-1 ) REPEAT = .TRUE.
C
C  Open DESCRIBE.FA   
      CALL OPENF(ICDCFA,IDESFA,1,512,2,IER)
C
       unityes=.false.
      if(iunit.eq.60)unityes=.true.
      MACTS=0
C      JERE = ICARD(79)
      ICHAN=53
C.....GET INFO ON SPECIFIED ACTIVE DATA
      macts=-1
      CALL DAGET(ICARD,ICARDP,ITABLE,40,MACTS)
      if(macts.eq.0)icardp=0
C
C.....CHECK FOR ADDITIONAL PARAMS
      CALL PLPARM(ICARD,ICARDP,BORD,COLOR,TICS,DINC,FAST,IDISP,IER)
      IF (IER .NE. 0) GOTO 8988
      if(iunit.eq.60) unityes=.true.
C
C.....DID USER SPECIFY ANY ACTIVE DATA SETS?
C
C.....START THINGS ROLLING
C
C open <lastmap> to be drawn
C
      map(1)=2HLM
      map(2)=2h
      call openf(70,map,0,0,1,ier)
C
      IF(MACTS.NE.0) GO TO 45
C
C if quick plot asked for and no active ids - just do plot
C
        if(fast)go to 53
C
C.......NO SETS SPECIFIED.  PROMPT . . .
   25   WRITE(NPRNT,1003)
 1003   FORMAT(' Enter active ID(s) to plot (CR=Exit)'\)
        CALL READIN(ICARD,LENICD,ISTART,istop)
        IF ( ICARD(1).EQ.0 ) GO TO 8988
        CALL DAGET(ICARD,ISTART,ITABLE,40,MACTS)
C
C.....ANY DATA AT ALL TO PLOT ?
C
   45 IF(NSTRCT.EQ.0) GO TO 8002
C
C  INITIALIZE COLOR ASSOCIATION TABLE TO USE DEFAULTS WHEN VECGET IS CALLED
C
          DO 191 MJ=1,MACTS
            KCOLOR(MJ)=-1
191       CONTINUE
        IF (.NOT. COLOR) GOTO 41
C
C  KEYWORD 'COLOR' PRESENT ON COMMAND LINE, THUS CHECK FOR USER ASSIGNMENTS
C
        SCOLOR=.TRUE.
        IMAC=0
        J=0
C
C.... CHECKING FOR COLOR KEYWORD
C
          DO 199 KJ=1,80
            J=KJ
187         IF ( ICARD(J) .EQ. 67 .OR. ICARD(J) .EQ. 99) THEN
188           J=J+1
              IF ((ICARD(J).GE.76.AND.ICARD(J).LE.82) .OR.
     +            (ICARD(J).GE.108.AND.ICARD(J).LE.114)) GOTO 188
              GOTO 193
            ENDIF
199       CONTINUE
C
C ....  NO COLORS TO OVERRIDE, THUS USE DEFAULT COLORS
C
          GOTO 41
C
C...    COLOR OVERRIDE TRUE, PROCEED WITH COLOR CODE SEARCH
C
193       IF(ICARD(J+1) .NE. 0)THEN
            L=J 
            DO 202 KK=1,MACTS
              LP=L
C.... ICOL is dummy variable, not used here
              CALL INUM(ICARD,ICOL,L)
              IF(L .NE. LP+1) THEN
                IMAC=IMAC+1
              ENDIF
202         CONTINUE
            IF (IMAC.EQ.0) IMAC=IMAC+1
          ELSE
            GOTO 492
          ENDIF
          IF(IMAC .GE. 1 .AND. IMAC .LE. MACTS) THEN
            IPNTR=J
            DO 399 LL=1,IMAC
              CALL INUM(ICARD,NCOLOR,IPNTR)
              IF (NCOLOR .EQ. 16) THEN
                KCOLOR(LL)=-1
              ELSE
                KCOLOR(LL)=NCOLOR
              ENDIF
399         CONTINUE
          ENDIF
492       IF(IMAC .EQ. MACTS) SCOLOR=.FALSE.
          IF (SCOLOR) THEN
C
C  FIND THE MAP ID THAT NEEDS A COLOR ASSIGNMENT
C
C....ELIMINATE THOSE IDs WHICH ALREADY HAVE A COLOR ASSIGNED
C
C....LCOL is a dummy variable, not used any where
            IPNTR=1
            DO 512 IL=1,IMAC
              CALL INUM (ICARD,LCOL,IPNTR)
512         CONTINUE
C
            DO 901 M=IMAC+1,MACTS
              CALL INUM(ICARD,MAPID,IPNTR)
555           WRITE(NPRNT,599) MAPID
599           FORMAT(1x,' Enter color for map ',I2,' (CR=MENU)'\)
              CALL READIN(NCARD,80,ISTART,istop)
              IF(NCARD(1).EQ.0)THEN
                WRITE(NPRNT,699)
699             FORMAT(7X,'1 = White',T40,'9  = Gray',/
     1              ,T8,'2 = Blue',T40,'10 = Dark Green',/
     2              ,T8,'3 = Yellow',T40,'11 = Light Red',/
     3              ,T8,'4 = Green',T40,'12 = Light Cyan',/
     4              ,T8,'5 = Red',T40,'13 = Light Magenta',/
     5              ,T8,'6 = Cyan',T40,'14 = Dark Blue',/
     6              ,T8,'7 = Magenta',T40,'15 = Dark Gray',/
     7              ,T8,'8 = Brown',T40,'16 = Default'\)
                CALL READIN(NCARD,80,ISTART,istop)
              ENDIF
              CALL INUM(NCARD,NCOLOR,1)
              IF (NCOLOR.GT.16.OR.NCOLOR.LT.1) THEN
                WRITE(NPRNT,801)
801             FORMAT(1x,'***You have entered an UNDEFINED',
     +                    ' COLOR NUMBER***')
                GOTO 555
              ELSEIF(NCOLOR .EQ. 16) THEN
C  FLAG FOR DEFAULT
                KCOLOR(M)=-1
              ELSE
                KCOLOR(M)=NCOLOR
              ENDIF
901         CONTINUE
          ENDIF
41      ABORT = .TRUE.
      DO 46 I = 1, MACTS
   46   IF (IDATYP((ITABLE(I)/7)+1).GE.6 .AND.
     +      IDATYP((ITABLE(I)/7)+1).LE.8) ABORT = .FALSE.
C
C  initialize plot variables and devices
C
      call initt(1,iunit,ifile)
C
C.....GET CORRECT WINDOW
      CALL SETWND
      CALL WHWND(IX1,IY1,IX2,IY2)
      IF (IX2-IX1 .EQ. 0.0  .OR.  IY2-IY1 .EQ. 0.0) GO TO 8001
C
      if(unityes)then
        if(iunit.eq.60)then
          write(*,*)
          write(*,898)(ifile(il),il=1,5)
898       format(/,' Plot will be drawn to a file named: ',
     +    5a2,', is this correct?  [(Y)/N]'\)
          read(*,'(a1)')iwait
          if(iwait.eq.'N'.or.iwait.eq.'n') then
            write(*,*)
            write(*,*)'Two ways to send a plot to a disk file:'
            write(*,*)' 1) Implicitly via the DEVICE option of the'
            write(*,*)'    PLOT command (output device is temporarily'
            write(*,*)'    the disk drive),'
            write(*,*)' 2) Explicitly via the DEVICE command (output'
            write(*,*)'    device is permanently the disk drive until'
            write(*,*)'    changed by the user).'
            write(*,*)
            go to 8988
          endif
        endif
      endif
C
C      call newpag
C
C go plot this stuff
C
      CALL VECGET(ier)
      if(ier.ne.0) go to 99
C
C.....PLOT BORDER, DO TICS, DO GRAPHICS DISPLAY
   52 CONTINUE
      call vecmod
      IF(BORD) CALL BDRAW
C.....TICS
C      IF(TICS) CALL TICKET(IDDT,MACTS,DINC,IER)
C.....GRAPHICS DISPLAY
  53  IF(FAST)then
       CALL GTNAM(-1,16,IDISP,MAP,IER)
       CALL OPENF(ICHAN,MAP,1,0,1,IER)
       if(IER .NE. 0) GO TO 85020
       ICOLOR = 1
       CALL DRFAST(ICHAN,IER)
       CLOSE(ICHAN,iostat=IER)
      Endif
C
C.....ALL DONE
C
      JERE=JERE+1
C      IF ( IBATCH.EQ.0 .AND. REPEAT ) GO TO 25
      GO TO 99
C
C.....ERRORS
C
 8001 Write(nprnt,80010)
80010 format(/,1x,'Display window not set.  Set window to ',
     +' active map(s).')
      GO TO 99
 8002 Write(nprnt,80020)
80020 format(/,1x,'No data to process. You must SELECT some ',
     +'map data.')
      GO TO 99
C
85020 CONTINUE
      WRITE(NPRNT,98502) (MAP(I),I=1,10),IER
98502 FORMAT(' *PLOT*  ERROR OPENING MAP ',10A2,'  ERROR',I5)
      JERE = JERE + 1
C
   99 CONTINUE
      CLOSE(ifchan,iostat=IER)
      call plotdv(0,470,999,idev)
      if(iunit.lt.60)then
        read(*,'(a1)')iwait 
        write(*,*)char(27),char(12)
      elseif(.not.unityes)then
        iunit=6
        ifile(1)=0
        call outcm
      endif
8988  continue
      END
