           SUBROUTINE VECGET(ier)
C*******************************************************************************
C
C          S U B R O U T I N E      V E C G E T
C FUNCTION:  To retrieve the active data for plotting
C
C
C*******************************************************************************
C
$INCLUDE: 'IO.INC'
$INCLUDE: 'STRNG.INC'
$INCLUDE: 'TYPE.INC'
$INCLUDE: 'WORK.INC'
       COMMON /COLOR/ ICOLOR,KCOLOR(40)
       COMMON /IDEVIC/ LDEVL(128)
       COMMON /FONTY/ KFONT,XORG,YORG,XINC,YINC,FSCAL,FLOOD
       COMMON /PLACT/ MACTS
       COMMON /TABLE/ ITABLE(40)
       COMMON /FLS/   IUNIT, IFILE(5)
       common/intrvl/ci(20),smin(20),smax(20)
       common/thick1/dfltpn,ntim,pnwd
      COMMON /LOCAL/ ICHAN,ISUB,JERE,WIND(4),IX1,IX2,IY1,IY2,
     +                IPOT1,ISTOP,ICONV,KNEW,NTOTAL,INDEX,IEX1,IEX2,
     +              XXLEN,YYLEN,XR,YR,XOFF,YOFF,TOTPIX,BIGAREA,IDD,IDDT
C
      INTEGER*2 PARENT(8),CHILD(8),mapname(25),KCARD(80),KBUFF(80)
     +          ,lcard(80)
      REAL IX1,IY1,IX2,IY2
      LOGICAL MCOMP,FLOOD, LTIME
C  VECTOR
      EQUIVALENCE (XXCEN,IBUFF(11)),(YYCEN,IBUFF(13))
      EQUIVALENCE (AAREA,IBUFF(7))
C  TEXT
      EQUIVALENCE (IBUFF(15),XSTA),(IBUFF(17),YSTA)
      EQUIVALENCE (IBUFF(7),WIDTH),(IBUFF(9),HEIGHT)
      EQUIVALENCE (IBUFF(11),ROT),(IBUFF(28),PENWID)
      equivalence (ibuff(88),vmin),(ibuff(90),vmax)
C
C  ENTER MAIN PLOTTING LOOP
C.......DISPLAY MIN AND MAX ELEVATIONS AND GET CONTOURING INTERVAL
       do 45 il=1,macts
       idd=itable(il)
       iddt=idatyp((idd/7)+1)
       if(iddt.ge.6.and.iddt.le.8)then
       call gtmap(icom,idd)
       call gtnam(idd,5,2h  ,map,ier)
       call adddir(map,mapname)
       call openf(11,mapname,1,256,2,ier)
       if(ier.ne.0)go to 85020
       call rdblk(11,1,ibuff,1,ier)
       close(11)
   2    WRITE(NPRNT,90002) (icom(l),l=1,10),VMIN,VMAX
90002   FORMAT(' Map: ',10a2,'  minimum:',F9.2,' maximum:',F9.2,/
     +    ' Enter contouring interval [, Minimum, Maximum ] '\)
        call readin(ICARD,80,ICARDP,LENICD)
        CALL INUM(ICARD,INTRVL,ICARDP)
        ci(il)=intrvl
C
C.......LOOK FOR USER SPECIFIED MINIMUM - BE CAREFUL OF "0.0" VERSES RETURN
C.......IF K = KK THEN ONLY A SPACE FOLLOWED INTERVAL, NOT "0"
        KK = ICARDP + 1
        CALL FNUM(ICARD,80,WMIN,ICARDP)
        smin(il)=wmin
C
C.......LOOK FOR USER SPECIFIED MAXIMUM - BE CAREFUL OF "0.0" VERSES RETURN
C.......IF K = KK THEN ONLY A SPACE FOLLOWED INTERVAL, NOT "0"
        KK = ICARDP + 1
        CALL FNUM(ICARD,80,WMAX,ICARDP)
        smax(il)=wmax
        LINES=LINES+2
        IF (INTRVL .GT. 0) GO TO 3
        WRITE(NPRNT,90003)IBEL,INTRVL
90003   FORMAT(A2,' NOT A REASONABLE INTERVAL - ',I5)
        GO TO 2
C
C.......REPLACE MAPS MIN AND MAX WITH USER SPECIFED VALUES
    3 CONTINUE
      endif
  45  continue
C
C
C      call newpag

      JCOL=1
      ICOLOR=0
      DO 50 I=1,MACTS
C
C.......CHECK IF PLOTTING CELL DATA
        IDD = ITABLE(I)
        IDDT= IDATYP((IDD/7)+1)
        IF(IDDT.EQ.9) GO TO 8003
C.......IF CELL MAP, GO TO CELL SWAP SECTION
        IF(IDDT.GE.6.AND.IDDT.LE.8) GO TO 55
C
C.......GET MAP NAME FROM STRUCTURE TABLE
        call clear(map,25) 
        CALL GTNAM(IDD,0,2h  ,MAP,IER)
C
C.......OPEN DATA CHANNEL 10 TO MAP
        call adddir(map,mapname)
        CALL OPENF(ICHAN,MAPname,1,256,2,IER)
        IF(IER.NE.0) GO TO  85020
        CALL RDBLK(ICHAN,1,IBUFF,1,IER)
        ICONV=0
        IF(MCOMP(IBUFF(77),'F',1,IER)) ICONV=1
        KNEW=IBUFF(119)
        KFONT=IBUFF(120)
        IF(IDDT.EQ.3.AND..NOT.FLOOD) KFONT=0
        IF(KFONT.GT.256) KFONT=1
        IF(IDDT.EQ.3.AND.FLOOD) GO TO 44
        IF(KFONT.EQ.0.AND.IDDT.NE.10) GO TO 44
C
C.......FONT MAP SPACE SETUP
        CALL plotsym(NPRNT,-61,0.0,0.0,0.0,0.0,
     +               4hfont,0.0,0.0,0.0,0,0,IER)
        CALL BLML(1,0,0.0,0.0,-30,IER)
        IF(IDDT.EQ.10.AND.KNEW.EQ.1) GO TO 44
        CALL SCALIT(XORG,YORG,XXLEN,YYLEN,XINC,YINC,XR,YR,10.0)
        XOFF=0.0
        YOFF=0.0
        CALL VWINDO(XOFF,XXLEN,YOFF,YYLEN)
        CALL SCRSET(XOFF,XXLEN,YOFF,YYLEN)
   44   CONTINUE
        CALL APGET(ITABLE(I),IPOT1,ISTOP,NTOTAL)
        IF(NTOTAL.EQ.0) GO TO 8002
C
C.......COLOR ASSIGN
C        
        IF( KCOLOR(I) .EQ. -1) THEN
C          IF( IBUFF(127) .EQ. 0 .AND. MACTS .GT. 1) THEN
            IF (I .EQ. MACTS) GOTO 201
C            IDD=ITABLE(I+1)
C            call clear(map,25) 
C            CALL GTNAM(IDD,0,2h  ,MAP,IER)
C            call adddir(map,mapname)
C            CALL OPENF(69,mapname,1,256,2,IER)
C            IF(IER.NE.0) GO TO  85020
C            CALL RDBLK(69,1,KBUFF,1,IER)
201         IF ( JCOL .EQ. ICOLOR) THEN
              JCOL=JCOL+1
C              IF (JCOL .NE. KBUFF(127) .OR. I .EQ. MACTS) THEN
                ITCOLOR=JCOL
C              ELSE 
C                ITCOLOR=JCOL+1
C              ENDIF
            ELSE
C              IF (JCOL .NE. KBUFF(127) .OR. I .EQ. MACTS) THEN
                ITCOLOR=JCOL
C              ELSE
C                ITCOLOR=JCOL+1
C              ENDIF
            ENDIF
C          ELSEIF (IBUFF(127) .EQ. 0 .AND. MACTS .EQ. 1) THEN
C            ITCOLOR=JCOL
C          ELSE
C            ITCOLOR=IBUFF(127)
C          ENDIF
C          CLOSE(69,iostat=IER)
        ELSE
          ITCOLOR=KCOLOR(I)
        ENDIF
C
C.......SET UP FOR COLOR TERMINAL
c        TOTPIX=LDEVL(7)*LDEVL(8)
c        BIGAREA=(IX2-IX1)*(IY2-IY1)
c        IF(ICONV.EQ.0) BIGAREA=BIGAREA/4046.
c        IF(ICONV.EQ.1) BIGAREA=BIGAREA/43260.
c        BIGAREA=BIGAREA/TOTPIX
C
C.......GET POINTER INFO FOR DATA IN MAPPTS
C
        DO 49 J=IPOT1,ISTOP
          call newpag
C
C.........GET DATA POINTER
          CALL GETFAST(J,WIND,INDEX,ISUB,IEX1,IEX2,IER)
          IF(IER.NE.0) GO TO 49
C
C.........DOES DATA FALL IN THE WINDOW?
          IF(IDDT.NE.10  .AND.
     +       INVWND(WIND(1),WIND(2),WIND(3),WIND(4),
     +              IX1,IX2,IY1,IY2).EQ.0) GO TO 49
C
C.........GET DATA AND PLOT
          CALL RDBLK(ICHAN,INDEX,IBUFF,1,IER)
          IF(IDDT.NE.10) GO TO 47
          IF(IBUFF(2).EQ.0) GO TO 47
          IF(IBUFF(27).GT.0) then
            pnwd=penwid
            if(penwid.lt.0.0)pnwd=dfltpn
          endif
           ntim=ibuff(27)
          if(ntim.lt.1)ntim=1
          CALL plotsym(0,IBUFF(26),XSTA,YSTA,WIDTH,
     1               HEIGHT,IBUFF(40),0.0,0.0,ROT,0,IBUFF(25),IER)
   47     CONTINUE
C          call newpag
C          IF(IDDT.EQ.3.AND.FLOOD) CALL FLDPOL(ICHAN,INDEX,
C     1       XXCEN,YYCEN,BIGAREA,AAREA,KFONT)
          ICOLOR=ITCOLOR
          IF(IDDT.NE.10.AND..NOT.FLOOD) 
     +       CALL VECDRW(ICHAN,INDEX,WIND(1),WIND(3))
   49   CONTINUE
        CLOSE(ICHAN,iostat=IER)
        IF(IDDT.EQ.3.AND.FLOOD) GO TO 50
        IF(KFONT.EQ.0.AND.IDDT.NE.10) GO TO 50
      CALL plotsym(0,-999,0.0,0.0,0.0,0.0,5hfonts,0.0,0.0,0.0,0,0,IER)
        CALL BLML(1,0,0.0,0.0,-999,IER)
        IF(KNEW.EQ.1.AND.IDDT.EQ.10) GO TO 50
C        CALL VWINDO(XORG,XR,YORG,YR)
C        CALL SCRSET(XORG,XR,YORG,YR)
        GO TO 50
C
C.......RASTER PLOTTING:  DICHOTOMOUS, DISCRETE & CONTINUOUS CELL
C
   55   CONTINUE
C.......COLOR ASSIGNMENT
        IF (KCOLOR(I) .EQ. -1) THEN
          IF ( ICOLOR .EQ. JCOL) THEN
            ICOLOR=JCOL+1
          ELSE 
            ICOLOR=JCOL
          ENDIF
        ELSE
          ICOLOR=KCOLOR(I)
        ENDIF
C.......
        IF(IDDT .EQ. 8) THEN
          call  mcontpr(idd,iddt,i,ier)
C	 ELSE
C          call    mbidipr
 	ENDIF
C
C.....ON TO NEXT ID TO PLOT
   50 CONTINUE
      GO TO 99
C
C.....ERROR RETURNS
C
 8002 write(nprnt,80010)
80010 format(/,1x,'No data to plot.  You must SELECT first.')
      GO TO 99
C
 8003 WRITE(NPRNT,98003)
98003 FORMAT(' *PLOT*  CAN NOT PLOT MULTIVALUE MAPS')
      JERE =JERE + 1
      GO TO 99
C
85020 CONTINUE
      WRITE(NPRNT,98502) (MAP(I),I=1,10),IER
98502 FORMAT(' *PLOT*  ERROR OPENING MAP ',10A2,'  ERROR',I5)
      JERE = JERE + 1
      GO TO 99
C
85040 CONTINUE
      WRITE(NPRNT,98504) (CHILD(II),II=1,5),IER
98504 FORMAT(' *PLOT*  ERROR FROM SWAP OF ',5A2,'  ERROR',I5)
      JERE = JERE + 1
      GO TO 99
C
C.....WAY OUT USUALLY
C
   99 CONTINUE
      ier=0
      return
C
      END
