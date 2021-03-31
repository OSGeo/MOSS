C *****************************************************************************
C
C             P R O G R A M     Q U E R Y
C FUNCTION:
C          THIS ROUTINE ALLOWS THE USER TO POINT TO
C          A POINT, LINE, OR POLYGON ON THE SCREEN AND
C          FIND OUT WHAT IT IS
C
C *****************************************************************************
$Include:'mfast.inc'
$Include:'dbchan.inc'
$Include:'io.inc'
$Include:'work.inc'
$Include:'strng.inc'
$Include:'vwnd.inc'
$Include:'dnames.inc'
$Include:'xypnts.inc'
      common /output/ buffer(132),ipt,iwrd,string(2)
      COMMON /TYPE/   IDATYP(40),NACTS
      COMMON /RBX/    IRBGX,ICHX
      COMMON /UNIT/   CHANEL,ATRBTE,POLYGON,ADRESS
      COMMON /RBY/    IRBGY,ICHY
      COMMON /PLACT/  MACTS
      COMMON /TABLE/  ITABLE(40)
      COMMON /WHMAP/  ISTRCT(300),NSTRCT
      COMMON /PNTFET/ NPOINT
      COMMON /FLS/    IUNIT,IFILE(5)
      DIMENSION LABEL(80),ITRNS(2),MAP1(25),NAME(25),mapname(25)
      DIMENSION I1(2),XTR(5),IDS(353),YTR(5)
      DOUBLE PRECISION X11,Y11
      REAL IWIND(4)
      INTEGER CHANEL,ATRBTE,POLYGON,ADRESS,fchan,wrk(10),buffer,string
      LOGICAL FPOINT,FLINE,MCOMP,ATTOUT,EDIT,IREPEAT,ALREAD
      EQUIVALENCE (TEMP,ITRNS(1)), (XT,I1)
C
C  READ IN COMMON BLOCK
C
      CALL initl(ier)
      if(ier.ne.0)stop
C
C  GET CURRENT LINE NUMBER
C
      ipt=1
C
C initilize display
C
      map(1)=2HLM
      map(2)=2h
      jchan=81
      write(NPRNT,1) (MAP(l),l=3,25)
1     format(1x,'QUERY:MAP= ',25a1)
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
C  SET WINDOW
C
      CALL SETWND
C
C
      X1 = 0.
      Y1 = 0.
      IREPEAT = .TRUE.
C
C  GET INPUT PARAMETERS
C
      CALL QUPARM ( ATTOUT, EDIT, ALREAD, X1, Y1, IER )
      IF(EDIT.AND.MACTS.GT.1) GO TO 970
C
C  SET SEARCH TOLERANCE
C
      IF(IBW.EQ.0) ATOL=(AMAX1(XALEN,YALEN))/75.
      IF(IBW.EQ.1) ATOL=AMAX1(XLEN,YLEN)/75.
C
C  TELL USER TO POINT TO DESIRED OBJECT
C
   12 CONTINUE
      KFOUND = 0
      items=items+1
      WRITE(NPRNT,1012)
 1012 FORMAT(//,' MAP WILL RE-DRAW - then do the following:',
     +     /,' ** Use cursor keys to position the crosshairs - ',
     +     /,' ** Hit SPACE BAR to < QUERY > that position -'
     +     /,' ** Hit any other key to < quit >',/)
      write(*,4)
  4   format(//,1x,'< CR > to proceed or < Q > to quit'\)
      call readin(iget,1,ic,iend)
      if(iget.eq.ichar('Q'))go to 99
C
C  GET POINT
C
      rewind (jchan)
C erase first
      write(*,*)char(27),char(12)
      write(*,*)'QUERY:JCHAN= ',JCHAN
      call drfast(jchan,ier)
      if(items.gt.1) then
       rewind(70)
       call drfast(70,ier)
       backspace(70)
      endif
C
C  LOOP THROUGH THE DATA PRESENTLTY DISPLAYED ON THE
C  CRT AND SEE IF THE ITEM CAN BE FOUND
C
98      IFOUND=0
        loop=0
      DO 20 I=1,MACTS
C
C  GET IDD FOR THIS DISPLAYED DATA SET
C
        IDD1=ITABLE(I)
        IF (IDD1 .LE. 0) GO TO 20
        ITYPE=IDATYP((IDD1/7)+1)
        IF (ITYPE .GE. 6 .AND. ITYPE .LE. 8) GO TO 400
      if(i.eq.1)then
      CALL VCURSR(IT,X1,Y1)
      loop=loop+1
      IREPEAT = .FALSE.
C
C  TEST FOR SPACE
       IF (it.eq.32) IREPEAT = .TRUE.
       IF (.NOT.IREPEAT) then 
          write(*,*)char(27),char(12) 
          GO TO 99
       endif
C
C  DRAW BOX ABOUT QUERY POINT
C
      CALL BXCALC(XTR,YTR,5,ATOL)
      CALL SYMDRW(XTR,YTR,5,X1,Y1)
      call anmode
      write(*,*)char(27),char(12)
C
      endif
        CALL APGET(IDD1,IND1,ISTOP,NTOTAL)
C
C  GET MAP NAME FOR THIS ACTIVE MAP ID
C
        CALL GTNAM(IDD1,0,2h  ,NAME,IER)
C
C  OPEN DATA CHANNEL TO THIS MAP
C
        ICHAN=10
       call adddir(name,mapname)
       CALL OPENF(ICHAN,mapname,1,256,2,IER)
        IF(IER.NE.0) GO TO 900
C
C  IF ATTOUT IS TRUE, OPEN CHANNEL TO ATRIBUTE FILE.
C
        IF(.NOT.ATTOUT) GO TO 14
        CALL RDBLK(ICHAN,1,IBUFF,1,IER)
        IF(IBUFF(76).NE.1) GO TO 930
        CALL GTNAM(IDD1,4,2h  ,MAP,IER)
        CHANEL=ichan+20
        call adddir(map,mapname)
        CALL OPENF(CHANEL,mapname,1,256,2,IER)
        IF(IER.NE.0) GO TO 940
C
        CALL INITIAL
C
   14   CONTINUE
C
C  CHECK DATA SET I
C
       j=ind1-1
   10  j=j+1
       if(j.gt.istop)go to 450
C
C  SET ITEM FOUND INDICATOR TO ZERO
C
  141     IFOUND=0
C
C  GET DATA
C
          CALL GETFAST(J,IWIND,IDNDEX,ISTPT,IEX1,IEX2,IER)
          IF(IER.NE.0) GO TO 10
C
C IF TYPE IS 2,3, OR 4 CHECK FOR POINT IN WINDOW.
C IF POINT IS IN WINDOW, EXPAND THE DATA.
C
          IF(ITYPE.EQ.1) GO TO 15
          IF(INVWND(X1,X1,Y1,Y1,IWIND(1),IWIND(2),
     1       IWIND(3),IWIND(4)).EQ.0) GO TO 10
C
C POINT IN RECTANGLE FOR THIS ITEM.  EXPAND DATA
C
   15     CONTINUE
C
C  STORE SOME RATHER CRITICAL INFORMATION IN CASE THIS IS A
C  HIT
C
          CALL RDBLK(ICHAN,IDNDEX,IBUFF,1,IER)
          ITEMN=IBUFF(5)
          ICTPT=IBUFF(23)
          ISTPT=IBUFF(24)
          I1(1)=IBUFF(7)
          I1(2)=IBUFF(8)
          CALL igetis(IBUFF,IDS,IHH,ICHAN,IDNDEX)
          CALL CORDXY(IBUFF,NPTS,IDNDEX,IWIND(1),IWIND(3),ICHAN)
C
          IF(ITYPE.LE.0.OR.ITYPE.GT.4) GO TO 10
          GO TO(6,7,8,7),ITYPE
C
C  POINT TEST
C
    6     CONTINUE
          call spoints('X ',x,xt,1,1)
          call spoints('Y ',y,yt,1,1)
c          XT=X(1)
c          YT=Y(1)
          IF(FPOINT(XT,YT,X1,Y1,ATOL))IFOUND=1
          GO TO 16
C
C  LINE TEST
C
    7     CONTINUE
          IF(FLINE(X1,Y1,NPTS,ATOL,KPOINT)) IFOUND=1
          GO TO 16
C
C  POLYGON TEST
C
    8     CONTINUE
          CALL SETPTR( IHH, IDS, IH )
          IH=IH+1
          IDS(IH)=NPTS
          IDS(1)=0
          X11=DBLE(X1)
          Y11=DBLE(Y1)
          CALL PIP(X11,Y11,INPOL,NPTS,IH,IDS)
          IF(INPOL.EQ.1) IFOUND=1
C
   16     IF(IFOUND.EQ.0) GO TO 10
C
C  PRINT OUT CHARACTERISTICS OF FOUND OBJECT
C
   30     CONTINUE
          KFOUND=KFOUND+1
          WRITE(NPRNT,1000)
 1000     FORMAT(/37H  ITEM HAS FOLLOWING CHARACTERISTICS  /)
          CALL GETSUB(ICHAN,ISTPT,IBUFF,MAP,ITS)
          WRITE(NPRNT,1003) (MAP(KL),KL=1,15)
 1003     FORMAT(13H SUBJECT   =  ,15A2)
          WRITE(NPRNT,1004) (mapname(KL),KL=1,25),ITEMN
 1004     FORMAT(13H MAP NAME  =  ,25a2,15H ITEM NUMBER =  ,I5)
          write(*,1007)x1,y1
 1007     format(' POSITION COORDINATES = ',2(F14.2))
          IF(ITYPE.EQ.2.OR.ITYPE.EQ.4)WRITE(NPRNT,1005) XT
 1005     FORMAT(13H LENGTH    =  ,F10.2,7H MILES   )
          IF(ITYPE.NE.3) GO TO 41
          WRITE(NPRNT,1006) XT
 1006     FORMAT(13H AREA      =  ,F12.2,6H ACRES )
C
C  ATTRIBUTE DUMP
C
   41     CONTINUE
C
          IF(.NOT.ATTOUT) GO TO 40
          LINES=1
          LOOPST=1
          CALL CLEAR(ICARD,79)
          WRITE(NPRNT,411)
  411     FORMAT(' ENTER  0-FOR ALL  #-FOR SPECIFIC: [RETURN=0] '\)
          CALL READIN(ICARD,lenicd,icardp,iend)
          CALL INUM(ICARD,LOOPND,icardp)
          IF (LOOPND .GT. 0) LOOPST=LOOPND
          IF (LOOPND .EQ. 0) LOOPND=ATRBTE
          IF (LOOPND .LE. ATRBTE) GO TO 231
          WRITE(NPRNT,233)LOOPND
  233 FORMAT(' WARNING:  REQUESTED ATTRIBUTE THAT DOES NOT EXIST ',I5)
          GO TO 41
C
  231     DO 25 JN=LOOPST,LOOPND
            CALL RDBLK(CHANEL,JN+1,IBUFF,1,IER)
            LOCATE=IBUFF(86)
            JATYPE=IBUFF(84)
            NWORDS=IBUFF(85)
            IF(JATYPE.EQ.3) JATYPE=-NWORDS
            CALL PACKC(IBUFF(2),MAP,10)
            DO 24 KN=1,10
              wrk(kn)=ibuff(1+kn)
              IF(wrk(kn).EQ.0) wrk(kn)=32
   24       CONTINUE
            call packc(wrk,map,10)
            CALL GETMAF(CHANEL,JATYPE,LOCATE,IDS,ITEMN,LABEL,VVAL)
C            LINES=LINES+1
            IF(JATYPE.EQ.1.OR.JATYPE.EQ.2) WRITE(NPRNT,1008)
     1        (MAP(KN),KN=1,5),VVAL
 1008       FORMAT(' KEY ',5A2,' VALUE ',F8.1)
            NCHARS=IABS(JATYPE)
            NCHARS=NCHARS/2
            IF(NCHARS.GT.15) NCHARS=15
            IF(JATYPE.LE.-1) WRITE(NPRNT,1009) (MAP(KN),KN=1,5),
     1        (LABEL(KN),KN=1,NCHARS)
 1009       FORMAT(' KEY ',5A2,' VALUE ',15A2)
            LINES = LINES + 2
C
C  IF EDIT IS TRUE, SEE IF USER WISHES TO EDIT VALUE
C
            IF(.NOT.EDIT) GO TO 25
C
            LINES = LINES + 1
            WRITE(NPRNT,1010)
 1010       FORMAT(' EDIT FIELD (Y/N) [N] '\)
            CALL READIN(LABEL,80,KK,iend)
            IF(.NOT.MCOMP(LABEL(1),'Y',1,IER)) GO TO 25
C
C  USER WISHES TO EDIT.  LETS DO IT.
C
C            LINES = LINES + 1
            JUMP=JATYPE
            IF(JATYPE.LE.0) JUMP=3
            WRITE(NPRNT,1011)
 1011       FORMAT(' NEW VALUE '\)
            CALL READIN(LABEL,80,KK,iend)
            GO TO (21,22,23),JUMP
   21       CALL INUM(LABEL,IVAL,KK)
            VAL=IVAL
            GO TO 23
   22       CALL FNUM(LABEL,80,VAL,KK)
C
   23       CONTINUE
            CALL PUTMAF(CHANEL,JATYPE,LOCATE,IBUFF,ITEMN,LABEL(1),VAL)
C
C  END OF ATTRIBUTE EDIT LOOP
   25     CONTINUE
          IF(.NOT.EDIT) GO TO 40
          CLOSE(ICHAN,iostat=IER)
          CLOSE(CHANEL,iostat=IER)
          GO TO 12
C
   40     CONTINUE
          IFOUND=0
          GO TO 450
C
C......CELL MAP QUERY ......
  400   CONTINUE
        CALL QUERCL(x1,y1,IDD1,loop,kfound,IER)
C        GO TO 20
C
C......KIND OF THE END
  450   CONTINUE
        IF (J .GE. ISTOP) GO TO 455
        J = J + 1
        GO TO 141
  455   IF(ATTOUT.AND.ITYPE.LE.5) CLOSE(CHANEL,iostat=IER)
        CLOSE(ichan,iostat=IER)
   20   CONTINUE
C
C  NOTHING FOUND.  TELL THE USER SO
C
      IF ( ALREAD .AND. KFOUND.LE.0 ) WRITE(NPRNT,1001)
 1001 FORMAT(' NO ITEM FOUND')
      IF ( ALREAD ) GO TO 99
      IF ( KFOUND.GT.0 ) GO TO 12
      LINES = LINES + 3
      WRITE(NPRNT,1002)
 1002 FORMAT(' NO ITEM FOUND.  TRY AGAIN')
      GO TO 12
C
C  CLEANUP FOR LEAVING
   99 CONTINUE
      CLOSE (icdcfa,iostat=IER )
      close(jchan,iostat=ier)
      close(ichan,iostat=ier)
      go to 9876
C
C ERROR RETURNS
C
  900 write(nprnt,901)(name(l),l=1,5)
  901 format(/,1x,'Error on accessing input map - ',5a2)
      KFOUND=1
      GO TO 99
  930 write(nprnt,931)(icom(l),l=1,10)
  931 format(/,1x,'This map does not have an attributes file -',10a1)
      GO TO 99
  940 write(nprnt,941)(map(l),l=1,5)
  941 format(/,1x,'Could not open attribute (.at) file -',5a2)
      GO TO 99
  950 write(nprnt,951)
  951 format(/,1x,'Could not open < DESCRIBE.FA > feature file')
      GO TO 99
  970 WRITE(NPRNT,3070)
 3070 FORMAT(' *EDITATT* CAN HAVE ONLY ONE ACTIVE MAP TO ATTRIBUTE',
     +' EDIT.')
      GO TO 99
C
9876  continue
      END
