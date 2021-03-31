       Subroutine Overlp(fname)
$Include:'dnames.inc'
$Include:'dbchan.inc'
$Include:'xypnts.inc'
$Include:'pqpnts.inc'
$Include:'xyxy.inc'
c**
$Include:'rspnts.inc'
       character*10 mapname
       Integer fname(25),clump(500),iunion(30)
c**
      COMMON /INCHAN/ KCHAN
      COMMON /BOPRMS/ IUD,NPTS1,NPTS2,NXYI,NLOBE,KLUE,NLIST,IFLLG
      COMMON /IPPY/ IPP(7)
$Include:'vwnd.inc'
      COMMON /OFFST/ XOFF,YOFF
      COMMON /OUTPUT/ KBUFF(128),IDUMMY(10)
      COMMON /IO/ NPRNT,IOIN
      COMMON /PNTFET/ NPOINT
      COMMON /WHMAP/ ISTRCT(300),NSTRCT
      COMMON /DOT/IH,JH,IDS(353),JDS(353)
$Include:'strng.inc'
      COMMON /TABLE/ ITABLE(40)
      COMMON /OSAVE/ ISUB(15),NPOLYS,LTYPE2,IDISP,NOSAVE,ICTSUB
      COMMON /WORK/ IBUFF(128),MAP(25),ICOM(10),JCOM(10)
      COMMON /TYPE/ IDATYP(40),NACTS
      COMMON /VALS/ TGR(2,2000)
      COMMON /VALS1/ KGR(2,2000),NEXT(2,2000),IFORK(2000)
      DIMENSION IHEAD(256),IPOINT(128)
      DIMENSION JTS(8),FXG(4),FXR(4)
      DIMENSION NAME1(25)
      DIMENSION LABL1(15),LABL2(15)
      integer unioned(30),mapnam(25)
      REAL IX1,IY1,IX2,IY2
      REAL IWIND1(4),IWIND2(4)
      Logical resolved,nosave
      EQUIVALENCE (JTS(1),IWIND2(1))
      data clump/500*0/
      data unioned/85,78,73,79,78,69,68,23*32/
C
C **********************************************************
C
C           S U B R O U T I N E   O V E R L P
C
C
C  FUNCTION:  RESOLVE OVERLAPPING POLYGONS ON AN INTRAMAP BASIS
C
C  INPUT PARAMETERS:
c
c  modified - Oct. 21 1987. (renamed to OVERLP) This version
c             accomodates the UNION command.  Major
c             modification is change to read overlapping polygons
c             from a single map instead of two maps and do a many
C             many polygon search and comparision.
C ***********************************************************
C
C
C  SET SOME VARIABLES
C
      XOFF=0.0
      YOFF=0.0
      INCORE=0
      NPOLYS=0
      nunioned=0
C
      ICHAN3=54
C
      kchan=52
      ICTSUB=51
      map(1)=2hZ
      CALL OPENF(ICTSUB,map,3,256,2,IER)
       map(1)=2hX
      CALL OPENF(kchan,map,3,0,1,IER)
       map(1)=2hA
       CALL OPENF(ichan3,map,3,256,2,IER)
C
C  Get pointer to features to be resolved
C
      Index=Itable(1)
      Call APGET(Index,istart,istop,npoly1)
      call gtmap(jcom,index)
C
C
       write (kchan)JCOM
C
       IFTYP1=3
C
C  GET CURRRENT DISPLAY WINDOW
C
      CALL WHWND(IX1,IY1,IX2,IY2)
C
C  CHECK FOR ZERO WINDOW
C
      IF(IX2-IX1.EQ.0.0.OR.IY2-IY1.EQ.0.0) go to 1902
C
C  GET NAME OF SOURCE MAPS
C
       CALL CLEAR(NAME1,25)
       DO 2 I=1,10
         NAME1(I)=JCOM(I)
  2    CONTINUE
C
C  WRITE MAP NAMES TO SAVE TEMP FILE
C
      WRITE (kchan) NAME1
      write (kchan) NAME1
C
C  OPEN INPUT FILES
C
      ICHAN1=10
      call adddir(name1,mapnam)
      CALL OPENF(ICHAN1,mapnam,1,256,2,IER)
       IF(IER.NE.0) go to 900
C
C  READ IN HEADER BLOCK FROM A MAP AND WRITE IT TO TEMP SAVE FILE
C
      CALL RDBLK(ICHAN1,1,IHEAD,1,IER)
      MINS=IHEAD(77)
      IHEAD(65)=IHEAD(77)
       write(kchan)(IHEAD(ll),ll=1,65)
C
       CALL RDBLK(ICHAN1,2,IHEAD(129),1,IER)
C
C  WRITE OUT DATA TYPE OF RESULTING DATA
C
       WRITE (kchan) IFTYP1
C
C  NOW ENTER MAIN COMPUTATION LOOP
C
C  This is a many to many comparision scheme

      npolys=0
      ipp(4)=0
      ipp(5)=0
      ipp(6)=0
      ipp(7)=0
      write(*,*)'--Working'
C
      DO 200 II=istop,istart,-1
C
C  Test if already resolved into a previous polygon
C
      if(clump(ii).gt.0)go to 200
C
C  set flag that this poly is a seed
C
      clump(ii)=2
C
      CALL GETFAST(ii,iwind1,irec,labl1,iattrb,ixtra,IER)
C
C  GET MBR FOR THIS ITEM
C
       CALL GETWND(irec,IWIND1,ICHAN1)
C
C  ITEM IN ANALYSIS AND DISPLAY WINDOW ?
C
      IF(INVWND(IWIND1(1),IWIND1(2),IWIND1(3),IWIND1(4),
     1 IX1,IX2,IY1,IY2).EQ.0) GO TO 200
C
C
C  GET ISLANDS
C
      CALL IGetis(IBUFF,IDS,IH,ICHAN1,irec)
C
C  GET LABEL
C
      KPOINT=IBUFF(24)
C
C  GET DATA
C
      FXG(1)=IWIND1(1)-xoff
      FXG(2)=IWIND1(2)-xoff
      FXG(3)=IWIND1(3)-yoff
      FXG(4)=IWIND1(4)-yoff
      IWIND1(1)=IWIND1(1)-XOFF
      IWIND1(2)=IWIND1(2)-XOFF
      IWIND1(3)=IWIND1(3)-YOFF
      IWIND1(4)=IWIND1(4)-YOFF
C
      CALL CORDXY(IBUFF,NPTS1,irec,IWIND1(1),IWIND1(3)
     1 ,ICHAN1)
C
      IC=NPTS1
      IPP(4)=0
      IPP(5)=0
      IDS(IH+1)=NPTS1
      IF(IH.EQ.0) GO TO 80
      CALL ISLWND(1,IH,IDS)
  80  CONTINUE
      IF(IH.GT.0) NPTS1=IDS(1)-1
      CALL GETSUB(ICHAN1,KPOINT,IBUFF,LABL1,ITS)
      do 104 il=1,15
  104 isub(il)=labl1(il)
C
C  NOW ENTER THE INNER COMPARISON LOOP
C
C
C start resolved flag as false
C
  106 resolved=.false.
C
      DO 100 JJ=istop,istart,-1
C
C   test if already looked at or  resolved - if yes, skip
      if(clump(jj).gt.0)go to 100
C
      call getfast(jj,iwind2,irec2,labl2,iattrb,ixtra,ier)
C
C  PULL INFO FROM RUNTIME MBR/SUBJECT FILE
C
       call getwnd(irec2,iwind2,ichan1)
       KPOINT=IBUFF(24)
       call getsub(ichan1,kpoint,ibuff,labl2,its)
C
C  get data for this poly and test if intersectioned with seed poly
C
       call rdblk(ichan1,irec2,ibuff,1,ier)
C
C  GET DATA TYPE, ITEM #, SUBJECT #
C
       ITEM2 = IBUFF(5)
C
C
C  GET ISLAND POINTERS FOR SECOND POLYGON
C
      call igetis(ibuff,jds,jh,ichan1,irec2)
C
C  GET POINT DATA
C
      NPTS2=0
      IF(IH.GT.0) NPTS2=1
      call cordpq(ibuff,npts2,irec2,iwind2(1),iwind2(3),ichan1)
c
      IPP(4)=0
      IPP(5)=0
      IPP(6)=0
      IPP(7)=0
      JDS(JH+1)=NPTS2
      IF(JH.EQ.0) GO TO 85
      CALL ISLWND(2,JH,JDS)
  85  CONTINUE
       IF(JH.GT.0) NPTS2=JDS(1)-1
C
C  DECREASE SEARCH MBR FOR unionCOMPUTATION
C
      FXR(1)=AMAX1(IWIND2(1),FXG(1))
      FXR(2)=AMIN1(IWIND2(2),FXG(2))
      FXR(3)=AMAX1(IWIND2(3),FXG(3))
      FXR(4)=AMIN1(IWIND2(4),FXG(4))
C
C  INITAILIZE SIGN & PASSES THROUGH BOOLE
C
       IPASSES = 0
       IPSIGN = -1
C
 96   IFLLG=0
      KLUE=0
      NXYI=0
      IUD=2
      NLOBE=0
      CALL BOOLE(FXG,FXR,16000,
     1           KGR,TGR,NEXT,IFORK,2000,KERR)
C
C A TRUE UNION
C
      IF(KERR.EQ.3) GO TO 99
      IF(KERR.NE.0) GO TO 100
      IF(KLUE.EQ.1) GO TO 100
C
C YUP.  FIGURE IT ALL OUT
C
C  set flag for this poly as resolved and set resolved as true
C
      knt=0
      clump(jj)=3
      resolved=.true.
C
      DO 504 IC=1,NXYI
      call spoints('X1',xy1,xt,ic,1)
c                           XT=ABS(XY1(IC))
                             xt=abs(xt)
      IF(XT.EQ.0.0) GO TO 505
      KNT=KNT+1
C increases seed mbr to encompass new poly
      FXG(1)=AMIN1(FXG(1),XT)
      FXG(2)=AMAX1(FXG(2),XT)
       call spoints('X ',x,xt,ic,2)     
c                    x(ic)=xt
       call spoints('X2',xy2,xt,ic,1)
c                            XT=XY2(IC)
      FXG(3)=AMIN1(FXG(3),XT)
      FXG(4)=AMAX1(FXG(4),XT)
       call spoints('Y ',y,xt,ic,2)
c                    y(ic)=xt
 504  CONTINUE
 505  CONTINUE
      IDS(IH+1)=KNT
      NPTS1=KNT
C
C be sure of subject
C
      do 107 il=1,30
  107 iunion(il)=unioned(il)
C
      GO TO 100
C
  99   CONTINUE
C
C reset clump due to bad union try
C do not write out
C
       Do 188 il=istart,istop
  188  if(clump(il).eq.3)clump(il)=0
       clump(ii) = 0
       go to 200
C
C      IF(KLUE.EQ.2) GO TO 200
 100  CONTINUE
C
C  test if another pass at this cluster is necessary
C
      if(resolved)go to 106
 159  CONTINUE
C
C load in set subjeect - say it was a unioned polygon
C
      if(knt.gt.0)then
       nunioned=nunioned+1
       iunion(9)=48+nunioned
       call packc(iunion,isub,30)
       knt=0
      endif
      ih=1
      if(npts1.le.2) go to 200
      call inout2(kchan,npts1,ids,ih,2)
      do 189 il=istart,istop
  189 if(clump(il).eq.3)clump(il)=1
C
 200  CONTINUE
C
C  ANY NEW POLYGONS? IF NOT, PRINT MESSAGE AND RETURN
C
      write(*,*)'Number of Polygons to Union=',npoly1
      write(*,*)'Number of polygons created =',npolys
c
      if(npolys.eq.0) go to 1903
C
C  DO DUMMY WRITE AS EOF TO TEMP FILE
C
      NOL=0
      WRITE (kchan) (ISUB(I),I=1,15),(ISUB(I),I=1,5),NOL,NOL
C
C  SAVE MAP
C
       CLOSE(ichan1,iostat=IER)
c
      call openf(ichan1,fname,0,256,2,ier)
      if(ier.ne.0) go to 1901
      call SAVWRK(ictsub,npolys,kchan,ichan1)
      close(ichan3)
C      ICHAN3=2
C      CALL OPENF(ICHAN3,iwork,1,512,2,IER)
C      IF (IER .NE. 0) go to 1904
      LTYPE1=3
c**
C      call openf(ichan1,fname,1,256,2,ier)
C      if(ier.ne.0) go to 1901
C
      CALL RDBLK(ichan1,1,IHEAD,1,IER)
      CALL WRBLK(ichan1,2,IHEAD(129),1,IER)
      CALL TUPDIR(ITP1,ITP2,ITP3,fname,IHEAD,iwork,ICwork,IER)
C
      close(icwork)
      return
C
 999  CONTINUE
C
C  PRINT OUT PROCESSING STATISTICS
C
 900  write(*,*)' Error accessing input map'
      return
 1901 write(*,*)' Error opening new map'
      return
 1903 write(*,*) ' No new polygons '
      return
 1904 write(*,*) 'Unable to open work directory'
      return
 1902 write(*,*) 'Display window not set.  Set to active map id(s)'
      return
C
      END
