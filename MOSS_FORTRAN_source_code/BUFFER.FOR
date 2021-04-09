$Include:'dbchan.inc'
$Include:'dnames.inc'
      common /dbmap/dbnmap(2)
      COMMON /BUFSTF/ ICHAN,BUFZ
      COMMON /PNTFET/ NPOINT
      COMMON /IO/           NPRNT       , IOIN
      COMMON /WHMAP/        ISTRCT(300) , NSTRCT
      COMMON /TABLE/        ITABLE(40)
$Include:'strng.inc'
      COMMON /TYPE/    IDATYP( 40 )    , NACTS
$Include:'work.inc'
      COMMON /OSAVE/ISUB(15),NPOLYS,LTYPE2  , IDISP  , NOSAVE, ICTSUB
      COMMON /OFFST/    SXMIN  , SYMIN
$Include:'xypnts.inc'
C      COMMON /CLUPPY/ KITE(250), XC1(250),YC1(250),
C     + XC2(250),YC2(250)
      integer dbnmap
      DIMENSION IHEAD(256),KCOM(5),IBUF(128),mapname(25),mapout(25)
$Include:'rspnts.inc'
$Include:'pqpnts.inc'
C      COMMON /WORK1/  R(1500),S(1500),P(750),Q(750)
      REAL MBR1(4)
      INTEGER  IBUFR(10), IDUM(1), IDS(99), mbr(8)
      LOGICAL OVER,MCOMP,nosave
      EQUIVALENCE (IBUF(1),Q(1)),(mbr1(1),mbr(1))
      DATA IBUFR/2HBU,2HFF,2HER,7*2H   /
      IDISP=0
C
C     ********************************************************
C
C        P R O G R A M   B U F F E R
C
C     THE FUNCTION OF THIS ROUTINE IS TO DRAW BUFFER ZONES ABOUT
C     A POINT, LINE OR POLYGON
C
C     THE USER THEN HAS THE OPTION OF HAVING THE RESULTING
C     BUFFERED AREA DISPLAYED ON THE CRT, SAVED IN THEIR
C     WORKFILE OR BOTH.
C     ************************************************************
C
C   READ IN COMMON
C
      CALL initl(ier)
      if(ier.ne.0)stop
C
C   CHANNEL NO.S FOR FILES
C
      ICTSUB=51
      ICHAN=10
      JCHAN=icwork
      KCHAN=52
      iovchn=53
C
C     SET SOME VARIABLES
C
      LINES = 1
      NOWAY = 0
C     SAVE STATUS OF WORKFILE DATABASE POINTERS
C
      IPROMT = 0
C
C   OPEN POINTER FILE
C
      CALL OPENF(icdcfa,idesfa,1,512,2,IER)
      IF(IER.NE.0) GO TO 901
C
      map(1)=2hZ
      CALL OPENF (KCHAN,map,3,1,1,IER)
      IF(IER.NE.0) GO TO 908
      map(1)=2hX
      CALL OPENF (ICTSUB,map,3,256,2,IER)
      IF (IER.NE.0) GO TO 908
C
C  PARSE USER INPUT
C
      CALL BUFCOM(kchan,MACTS,BUFZ,LINES,IER)
C
      IF(IER.NE.0) GO TO 1000
C
C
C     GET TOTAL NUMBER OF NEW ITEMS FOR THIS SAVE FILE
C
      MNDEX = ITABLE(1)
C
C     CHECK PARAMETER INPUT
C
      IF (MACTS.EQ.0) GO TO 390
      IF (IDATYP(MNDEX/7 + 1).EQ.6) GO TO 410
C
C
C     GET CURRENT WINDOW
C
      CALL WHWND (VX1,VY1,VX2,VY2)
      IF (VX2-VX1 .EQ. 0.0 .OR. VY2-VY1 .EQ. 0.0) GO TO 911
C
C     CONVERT MILES TO METERS
C
      VX11=VX1-BUFZ
      VY11=VY1-BUFZ
      SXMIN=VX11
      SYMIN=VY11
C
C     SET ITEM COUNTER
C
      NPOLYS = 0
C
C
C     GET POINTER TO DATA POINTER IN MAPPTS FOR ACTIVE
C     DATA SET I
C
      IDD=ITABLE(1)
      CALL APGET (IDD,ISTART,ISTOP,NTOTAL)
C
C   OPEN CHANNEL TO MAP FILE
C
      CALL GTMAP(ICOM,IDD)
      call adddir(icom,mapname)
      CALL OPENF(ICHAN,mapname,1,256,2,IER)
C
C  PERFORM FINAL CORRECTION TO ZONE SPECIFICATION (BASED ON COORD CONV)
C
      CALL RDBLK(ICHAN,1,IHEAD,1,IER)
      ihead(77)=77
      IF(MCOMP(IHEAD(77),'F',1,IER)) CALL UDCONV(1,4,BUFZ,BUFZ1,IER)
      IF(MCOMP(IHEAD(77),'F',1,IER))  BUFZ = BUFZ1
C
C   WRITE OUT INFO TO SAVE TEMP FILE
C
      WRITE (kchan) (mapname(il),il=1,25)
      write (kchan) (mapname(il),il=1,25)
      IHEAD(65)=IHEAD(77)
      write (kchan)(IHEAD(ll),ll=1,65)
        CALL RDBLK(ICHAN,2,IHEAD(129),1,IER)
        IFTYP=IDATYP(IDD/7+1)
        IFT=3
      write (kchan)IFT
C
C     LOOP THROUGH FOR ACTIVE DATA SET I
C
      DO 350 J = ISTART,ISTOP
C
C  GET DATA POINTER
C
          CALL GETFAST(J,mbr,INDEX,isubj,iattrb,ixtra,IER)
          IF(IER.NE.0) GO TO 350
C
C     GET A WINDOW FOR A DATA ITEM
C
          CALL GETWND (INDEX,MBR1,ICHAN)
C
C     IS THE ITEM IN THE DISPLAY WINDOW?
C
          IF (INVWND(MBR1(1),MBR1(2),MBR1(3),MBR1(4),VX1,VX2,VY1,VY2)
     1    .EQ.0) GO TO 350
C
C     ITS IN THE WINDOW.  EXPAND THE DATA
C
          ITEMN=IBUFF(5)
          LTYPE2=IBUFF(6)
          IPOINT=ibuff(24)
          NSLAND=IBUFF(26)
C
C  SAVE SUBJECT
C
          CALL GETSUB( ICHAN,IPOINT,IBUF,ISUB,NITMS)
          DO 202 I=1,5
              ISUB(10+I)=IBUFR(I)
  202     CONTINUE
C
C
C  GET COORDINATE DATA
C
      CALL IGETIS(IBUFF,IDS,NSLAND,ICHAN,INDEX)
          XM=MBR1(1)-SXMIN
          YM=MBR1(3)-SYMIN
          CALL CORDXY(IBUFF,NPTS,INDEX,XM,YM,ICHAN)
C
C  TRAP FOR SPECIAL CONDITIONS
C
      IF(LTYPE2.EQ.1.AND.NPTS.GT.1) NPTS=1
      IF(LTYPE2.EQ.2.AND.NPTS.EQ.1) LTYPE2=1
      call spoints('X ',x,x1,1,1)
      call spoints('X ',x,x2,2,1)
      call spoints('Y ',y,y1,1,1)
      call spoints('Y ',y,y2,2,1)
      IF(LTYPE2.EQ.2.AND.NPTS.EQ.2.AND.
     1 (SQRT((X1-X2)**2+(Y1-Y2)**2).LT.0.001)) LTYPE2=1
C
C    ELIMINATE ISLANDS
C
  204     IF((LTYPE2.NE.3).OR.(NSLAND.EQ.0)) GO TO 205
      NPTS=IDS(1)-1
C
C  CHECK FOR too many POINTS IN COORDINATE STRING
C
 205  CONTINUE
      IF(NPTS.GT.32700) GO TO 910
C
C     *******************************************
C
C
C     DO PROPER BUFFER TYPE ACCORDING TO LTYPE2
C
          GO TO (210,230,300,230,210), LTYPE2
C
C     POINT
C
  210     CONTINUE
          NPTS=80
          call spoints('X ',x,xcent,1,1)
          call spoints('Y ',y,ycent,1,1)
c          XCENT=X(1)
c          YCENT=Y(1)
          CALL CIRCLE(NPTS,XCENT,YCENT,BUFZ)
C
C     REVERSE ORDER SO COORDINATES ARE CLOCKWISE
C
          DO 220 I=1,NPTS
          call spoints('X ',x,xi,i,1)
          call spoints('Y ',y,yi,i,1)
          call spoints('R ',r,xi,i,2)
          call spoints('S ',s,yi,i,2)
c          R(I)=X(I)
c          S(I)=Y(I)
  220     CONTINUE
          call spoints('R ',r,r1,1,1)
          call spoints('S ',s,s1,1,1)
          call spoints('R ',r,r1,npts+1,2)
          call spoints('S ',s,s1,npts+1,2)
c          R(NPTS+1)=R(1)
c          S(NPTS+1)=S(1)
          NPTS=NPTS+1
C
          GO TO 340
C
C     LINE OR ARC
C
  230     CONTINUE
 231      CONTINUE
          TOL=BUFZ/12.
          call spoints('X ',x,xnpts,npts,1)
          call spoints('X ',x,xnpts1,npts-1,1)
          call spoints('Y ',y,ynpts,npts,1)
          call spoints('Y ',y,ynpts1,npts-1,1)
          IF(XNPTS.EQ.XNPTS1.AND.YNPTS.EQ.YNPTS1) NPTS=NPTS-1
          IF(NPTS.GT.9) CALL BWEEDT(NPTS,TOL,NSLAND,IDS,R1,S1)
c          
c  do r and s points
c
          CALL VZONE(BUFZ,NPTS,1,NCOR1)
c
c  do p and q points
c
          CALL VZONE(-BUFZ,NPTS,2,NCOR2)
C
C  ROUND THE ENDS
C
      NC=10
      call spoints('R ',r,rncor1,ncor1,1)
      D1=XNPTS-RNCOR1
      IF(D1.EQ.0.0) D1=.001
      call spoints('S ',s,sncor1,ncor1,1)
      D2=YNPTS-SNCOR1
      DIST=SQRT(D1*D1+D2*D2)
      ANG1=ATAN2(D2,D1)
      call spoints('P ',p,pncor2,ncor2,1)
      call spoints('Q ',q,qncor2,ncor2,1)
      D1=XNPTS-PNCOR2
      D2=YNPTS-QNCOR2
      ANG2=ATAN2(D2,D1)
      ANG1=ANG1*57.2958
      ANG2=ANG2*57.2958
      CALL BARC(NCOR1,NC,ANG1,ANG2,DIST,XNPTS,YNPTS)
      NCOR1=NCOR1+NC
      call spoints('X ',x,x1,1,1)
      call spoints('Y ',y,y1,1,1)
      call spoints('P ',p,p1,1,1)
      call spoints('Q ',q,q1,1,1)
      call spoints('R ',r,r1,1,1)
      call spoints('S ',s,s1,1,1)
       D1=X1-P1
       IF(D1.EQ.0.0) D1=.001
       D2=Y1-Q1
       ANG1=ATAN2(D2,D1)
       D1=X1-R1
       D2=Y1-S1
       ANG2=ATAN2(D2,D1)
       ANG1=ANG1*57.2958
       ANG2=ANG2*57.2958
C
C     PLACE ALL LINE DATA IS ONE ARRAY.
C
          IS1=NCOR1+1
          IS2=NCOR1+NCOR2
          KOUNT=NCOR2+1
          NPTS=IS2
          DO 270 I = IS1,IS2
               KOUNT = KOUNT - 1
            call spoints('P ',p,pkount,kount,1)    
c                PKOUNT=P(KOUNT)
            call spoints('Q ',q,qkount,kount,1)
c                QKOUNT=Q(KOUNT)
      call spoints('R ',r,pkount,i,2)
      call spoints('S ',s,qkount,i,2)
c      R(I)=PKOUNT
c      S(I)=QKOUNT
  270    CONTINUE
C
          CALL BARC(NPTS,NC,ANG1,ANG2,DIST,X1,Y1)
       NPTS=NPTS+NC
C
C     REMOVE SELF CROSSING LOOPS FROM POLYGON
C
C
          DANGL=0.0
          NPTS2=NPTS-2
           CALL DELOOP(NPTS,15,R1,S1)
           CALL RLNCLN(NPTS,R1,S1)
C
C  CLOSE THE POLYGON
C
          NPTS = NPTS + 1
        
c        R1=R(1)
c        S1=S(1)
       call spoints('R ',r,r1,npts,2)
       call spoints('S ',s,s1,npts,2)
c       R(NPTS)=R1
c       S(NPTS)=S1
C
C  MAKE SURE POLYGON IS CLOCKWISE
C
          GO TO 340
C
C     POLYGON
C
 300      CONTINUE
          call spoints('X ',x,xnpts,npts,1)
          XT=XNPTS-.1
          call spoints('X ',x,xt,npts,2)
c                    x(npts)=xt
c
c  do r and s points
c
          CALL VZONE(BUFZ,NPTS,1,NCOR)
C
C     CLOSE THE BUFFERED POLYGONS
C
          NCOR = NCOR + 1
       call spoints('R ',r,r1,1,1)
       call spoints('R ',r,r1,ncor,2)
c        R(NCOR)=R(1)
       call spoints('S ',s,s1,1,1)
       call spoints('S ',s,s1,ncor,2)
c        S(NCOR)=S(1)
       call spoints('P ',p,p1,1,1)
       call spoints('P ',p,p1,ncor,2)
c        P(NCOR)=P(1)
       call spoints('Q ',q,q1,1,1)
       call spoints('Q ',q,q1,ncor,2)
c        Q(NCOR)=Q(1)
      NPTS=NCOR
C
C     REMOVE SELF CROSSING LOOPS FROM POLYGON
C
  320     CALL DELOOP(NPTS,15,R1,S1)
          CALL RLNCLN(NPTS,R1,S1)
C
C     IF NOSAVE IS TRUE, WRITE THE RESULT OF THE BUFFER
C     PROCESS TO THE WORKFILE
C
  340     CONTINUE
          NSLAND=1
          IF(NPTS.LE.2) GO TO 350
          CALL INOUT2(kchan,NPTS,IDUM,NSLAND,1)
  350 CONTINUE
C
C
C     IF NO ITEMS FALL IN THIS WINDOW, RETURN
C
      IF (NPOLYS.GT.0) GO TO 360
      WRITE (NPRNT,540)
      LINES = LINES + 2
      GO TO 1000
  360 CONTINUE
C
      close(ichan,iostat=ier)
      call openf(ichan,jcom,0,256,2,ier)
      CALL SAVWRK(ictsub,npolys,kchan,ichan)
C

C
C     UPDATE DIRECTORY
C
      CALL RDBLK(ICHAN,1,IHEAD,1,IER)
      CALL WRBLK(ICHAN,2,IHEAD(129),1,IER)

C
      CALL TUPDIR(ITP1,ITP2,dbnmap(2),jcom,IHEAD,iwork,icwork,IER)
      IF(IER.NE.0) GO TO 400
      NITEM=IHEAD(63)
C
      close(icwork)
C
      GO TO 1000
C
C     ERROR RETURNS
C
  390 WRITE (NPRNT,570)
      GO TO 420
  400 WRITE(NPRNT,550)
      GO TO 420
  410 WRITE (NPRNT,590)
  420 LINES = LINES + 2
      GO TO 1000
C
C
  440 FORMAT (32H ENTER ACTIVE DATA SET TO BUFFER)
  460 FORMAT (28H ENTER BUFFER ZONE IN MILES )
  470 FORMAT (19H PRESENT WINDOW =  ,4I10)
  510    FORMAT (37H AFTER POLY CORDOR CALL, NPTS,NCOR = ,2I5)
  530    FORMAT (9H LENTH = ,I10)
  540 FORMAT (19H NO ITEMS IN WINDOW)
  550 FORMAT(25H WORKFILE DIRECTORY FULL  )
  570 FORMAT (30H NO ACTIVE DATA SETS SPECIFIED)
 590  FORMAT(' USE THE CBUFFER COMMAND TO BUFFER CELL DATA')
C
C   ERROR ON OPENING A FILE
C
 901  Write(*,*)' Could not open <DESCRIBE.FA>'
      GO TO 1010
 907  Write(*,*)' Could not open project file <.dt>'
      GO TO 1000
 908  write(*,*)' Error on accessing <temp>'
      GO TO 1000
 909  write(*,*)' Error on accessing input map'
      GO TO 1000
 910   WRITE(NPRNT,3010) NPTS
 3010 FORMAT('ITEM HAS MORE THAN 32700 POINTS. NPTS = ',I5)
      LINES=LINES+2
      GO TO 1000
 911  write(*,*)' Display window not set. Set to active map id(s)'
      GO TO 1000
C
 1000   CONTINUE
1005  continue
1010  continue
      END
