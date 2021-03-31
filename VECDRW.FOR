      SUBROUTINE VECDRW (ICHAN,INDEX,XMIN,YMIN)
C*******************************************************************************
C
C            S U B R O U T I N E   V E C D R W
C FUNCTION:   THIS ROUTINE PLOTS THE CONTENTS OF AN ACTIVE id ON THE CRT
C
C PARAMETERS: ICHAN     IN  - THE MAP FILE OPENER.
C             INDEX     IN  - THE ENTRY SET INDEX.
C             IBUFF     IN  - THE MAP FILE NAME.
C             XMIN      IN  - MINIMUM X POINT.
C             YMIN      IN  - MINIMUN Y POINT
C
C***************************************************************************
$Include:'IO.inc'
$Include:'WORK.inc'
       COMMON /FONTY/ KFONT,XORG,YORG,XINC,YINC,FSCAL,FLOOD
       COMMON /FLS/   IUNIT,IFILEE(5)
$Include:'xypnts.inc'
       common/thick1/dfltpn,ntim,pnwd
       COMMON /RDATA/ ISLAND(350),JBUFF(128),IRP(4),NOTUSED(19)
C       DIMENSION IRP(4)
       INTEGER lcard(80)
       EQUIVALENCE(IRP(1),WID),(IRP(3),HEI)
       LOGICAL FLOOD
       IJCOUNT=IJCOUNT+1
C
C  GET ISLANDS
C
      call newpag
      ISLAND(1)=0
      CALL Igetis(IBUFF,ISLAND,NSLAND,ICHAN,INDEX)
      IF(NSLAND.LT.0) GO TO 930
      NSAVE=ISLAND(1)
C
C  GET COORDINATE DATA
C
      CALL SCAGET(IBUFF,SCALE)
      ITYPE=IBUFF(6)
      NPTS=IBUFF(25)
      NSUB=IBUFF(24)
C.....FEATURE CARTO ASSIGNMENTS  ALWAYS A ZERO UNLESS ASSIGNED
      ISYM = IBUFF(27)
      ntim = IBUFF(28)
C      ICOLOR = IBUFF(29)
      IRP(1) = IBUFF(30)
      IRP(2) = IBUFF(31)
      IRP(3) = IBUFF(32)
      IRP(4) = IBUFF(34)
C      IF((ITYPE.EQ.3.OR.ITYPE.EQ.13).AND.KFONT.NE.0) ICOLOR=1
      IF(KFONT.GT.0.AND.ITYPE.EQ.2) GO TO 40
C
C.....NPTS IS THE NUMBER OF POINTS IN THE DATA ENTRY
C.....NOW ENTER THE PLOT SEQUENCE
C
      IPOINT=1
      NREC=INDEX+1
      IF(NSLAND.GT.99) NREC=NREC+((NSLAND-99)/128)+1
      READ(ICHAN,REC=NREC,ERR=900,iostat=ier) IBUFF
      XPOINT=IBUFF(1)
      YPOINT=IBUFF(2)
C
      if(npts.eq.1)then
C
C draw a point
C        if(kfont.gt.0)go to 30
         call drplus((xpoint*scale)+xmin,(ypoint*scale)+ymin)
         return
      else
C
C move to first point
C
        CALL plotdv((XPOINT*SCALE)+XMIN,(YPOINT*SCALE)+YMIN,3,idev)
      endif
C
        IWORD=1
        INCC=2
        IF(ITYPE.GE.11.AND.ITYPE.LE.13) INCC=3
        DO 10 I=2,NPTS
          IWORD=IWORD+INCC
          IF(IWORD.GT.128.AND.INCC.EQ.2) GO TO 2
            IF(IWORD.GT.126.AND.INCC.EQ.3) GO TO 2
              GO TO 3
    2     IWORD=1
          NREC=NREC+1
          READ(ICHAN,REC=NREC,ERR=900,iostat=ier) IBUFF
    3     XPOINT=IBUFF(IWORD)
          YPOINT=IBUFF(IWORD+1)
          XPOINT=(XPOINT*SCALE)+XMIN
          YPOINT=(YPOINT*SCALE)+YMIN
          IF(ISLAND(1).EQ.0) GO TO 8
            IF(I.EQ.IABS(ISLAND(IPOINT))) GO TO 5
    8         CONTINUE
              CALL plotdv(XPOINT,YPOINT,2,idev)
              GO TO 9
    5     CALL plotdv(XPOINT,YPOINT,3,idev)
          IPOINT=IPOINT+1
          IF(ISLAND(IPOINT).EQ.ISLAND(IPOINT-1))IPOINT=IPOINT+1
          IF(IPOINT.GT.NSLAND) ISLAND(1)=0
    9     CONTINUE
   10   CONTINUE
        RETURN
C
C  PLOT WITH A FONT
C
   30 CONTINUE
      XT=(XPOINT*SCALE)+XMIN
      YT=(YPOINT*SCALE)+YMIN
      XT=XT-XORG
      YT=YT-YORG
      XT=(XT*XINC)
      YT=(YT*YINC)
C
C** CHECK FOR FEATURE CARTO ASSIGNMENT
C
      IF (ISYM .GT. 0  .AND. ISYM .LT. 128) GO TO 35
        READ(ICHAN,REC=NSUB) IBUFF
        ISYM=IBUFF(20)
        ntim = IBUFF(21)
        IRP(1) = IBUFF(22)
        IRP(2) = IBUFF(23)
        IRP(3) = IBUFF(24)
        IRP(4) = IBUFF(25)
C        ICOLOR=IBUFF(26)
C
   35 CONTINUE
C      IF(ICOLOR.LE.0) ICOLOR=1
C      CALL SETCOL(IUNIT,ICOLOR,IER)
      if(ntim.lt.1)ntim=1
      pnwd=.01
      IF (WID .LE. .02) WID = .3
      IF (HEI .LE. .02) HEI = .3
      CALL plotsym(1,KFONT,XT,YT,WID,HEI,ISYM,0.0,0.0,0.0,0,-1,IER)
      ntim=1
C      ICOLOR = 1
C      CALL SETCOL(IUNIT,ICOLOR,IER)
      RETURN
C
C  FONT ASSIGNED LINES?
C
   40 CONTINUE
      CALL CORDXY(IBUFF,NPTS,INDEX,XMIN,YMIN,ICHAN)
      NVERT=NPTS
      DO 45 I=1,NVERT
        call spoints('X ',x,xi,i,1)
        call spoints('Y ',y,yi,i,1)
        XT=XI-XORG
        YT=YI-YORG
        XT=XT*XINC
        YT=YT*YINC
        call spoints('X ',x,xt,i,2)
C                  x(i)=xt
        call spoints('Y ',y,yt,i,2)
C                  y(i)=yt
   45 CONTINUE
C
C  CHECK FOR FEATURE CARTO ASSIGNMENT
C
      IF (ISYM .GT. 0  .AND. ISYM .LT. 33) GO TO 48
        READ(ICHAN,REC=NSUB,ERR=900) IBUFF
        ISYM=IBUFF(20)
        ntim = IBUFF(21)
        IRP(1) = IBUFF(22)
        IRP(2) = IBUFF(23)
        IRP(3) = IBUFF(24)
        IRP(4) = IBUFF(25)
C        ICOLOR=IBUFF(26)
C
   48 CONTINUE
C      IF(ICOLOR.LE.0) ICOLOR=1
C      CALL SETCOL(IUNIT,ICOLOR,IER)
      if(ntim.lt.1)ntim=1
      pnwd=.01
      IF (WID .LE. .02) WID = .1
      IF (HEI .LE. .02) HEI = .15
      IF(ISYM.EQ.9.OR.ISYM.EQ.10.OR.ISYM.EQ.26.OR.ISYM.EQ.29)
     1    WID=WID/2.0
      CALL BLML(1,NVERT,WID,HEI,ISYM,IER)
C      ICOLOR=1
C      CALL SETCOL(IUNIT,ICOLOR,IER)
      ntim=1
      RETURN
C
  900 WRITE(NPRNT,3000) NREC
 3000 FORMAT(' *PLOTL*  ERROR IN READING RECORD',I5)
      ier=1
      RETURN
  930 write(nprnt,93010)
93010 format(/,1x,'Too many islands in a polygon feature')
      ier=1
99999      RETURN
      END
