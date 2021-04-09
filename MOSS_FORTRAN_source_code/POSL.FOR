      SUBROUTINE POSL(ISL1,ISL2,IFLAG)
      COMMON /BOPRMS/ KUD,NGG,NRR,NXYI1,KLOBE,KLUE1,NLIST,IFLGG
      COMMON /VALS/ TGR(2,200)
      COMMON /VALS1/ KGR(2,200),NEXT(2,200),IFORK(200)
      COMMON /DOT/ IH,JH,IDS(353),JDS(353)
$Include:'io.inc'
      COMMON /IPPY/ IPP(3),IOFX,IOFY,IOFP,IOFQ
      COMMON /PISTF/ INDEX,KNDEX,ICL,FXR(4),NXYI,NLOBE,KP,
     1               KP2,MAJ,MIN,III,IIII,JJJ,KH,ISTAR,ISTOP,NGT,NRT,
     2               IUD
$Include:'xypnts.500'
$Include:'pqpnts.500'
$Include:'xrxr.500'
$Include:'xyxy.500'
      DIMENSION ISL3(10),ISL1(1),ISL2(1),FXG(4)
C
C
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
C                   S U B R O U T I N E    P O S L
C
C  TAKE UNION OF ISLANDS (REQUIRED FOR INTERECTION WHEN
C  BOTH MAJOR AND MINOR POLYGONS HAVE ISLANDS)
C
C
C  INPUT PARAMETERS:
C
C          ISL1   = CANDIDATE ISLANDS FROM MAJOR POLYGON
C          ISL2   = CANDIDATE ISLANDS FROM MINOR POLYGON
C          IFLAG  = UNION/INTERSECTION FOUND FLAG (1=O.K.)
C          JUD    = NOT USED
C
C************************************************************************
C
      IFLAG=0
      NMIN=0
      II=0
C
C  ENTER MAIN COMPARISON LOOP
C
 200  CONTINUE
      II=II+1
      IF(II.GT.MAJ) GO TO 400
C
      INDEX=ISL1(II)
      IF(INDEX.LT.0) GO TO 200
      ISTAR=IDS(INDEX)
      ISTOP=IDS(INDEX+1)-1
      IF(INDEX.EQ.IH) ISTOP=ISTOP+1
C
C  GET MBR FOR MAJOR ISLAND
C
      KNDEX=(INDEX-1)*4+1
      call sspoints('XR',xr,fxg(1),kndex,1)
      call sspoints('XR',xr,fxg(2),kndex+1,1)
      call sspoints('XR',xr,fxg(3),kndex+2,1)
      call sspoints('XR',xr,fxg(4),kndex+3,1)
c      FXG(1)=XR(KNDEX)
c      FXG(2)=XR(KNDEX+1)
c      FXG(3)=XR(KNDEX+2)
c      FXG(4)=XR(KNDEX+3)
C
C
C  SHUFFLE ISLAND INTO OPEN SPACE ABOVE MAJOR POLYGON
C
      IC=0
      KL=ISTOP+1
      DO 10 J=ISTAR,ISTOP
      KL=KL-1
      IC=IC+1
      call sspoints('X ',x,xt,kl,1)
      call sspoints('Y ',y,yt,kl,1)
c      XT=X(KL)
c      YT=Y(KL)
       call sspoints('X ',x,xt,iofx+ic,2)
       call sspoints('Y ',y,yt,iofy+ic,2)
c      x(iofx+ic)=xt
c      y(iofy+ic)=yt
  10  CONTINUE
      NGG=IC
C
C  LOOP ON MINOR ISLANDS
C
      KITS=0
      DO 100 JJ=1,MIN
      INDEX=ISL2(JJ)
      IF(INDEX.LT.0) GO TO 100
      ISTAR=JDS(INDEX)
      ISTOP=JDS(INDEX+1)-1
      IF(INDEX.EQ.JH) ISTOP=ISTOP+1
C
C
      KNDEX=1419+(INDEX-1)*4+1
      IC=0
C
C  SHUFFLE ISLAND FOR MINOR ABOVE MINOR POLYGON IN P,Q
C
      KL=ISTOP+1
      DO 120 J=ISTAR,ISTOP
      IC=IC+1
      KL=KL-1
      call sspoints('P ',p,xt,kl,1)
      call sspoints('Q ',q,yt,kl,1)
c      XT=P(KL)
c      YT=Q(KL)
c       p(iofp+ic)=xt
c       q(iofq+ic)=yt
       call sspoints('P ',p,xt,iofp+ic,2)
       call sspoints('Q ',q,yt,iofq+ic,2)
 120  CONTINUE
      NRR=IC
C
      call sspoints('XR',xr,fxr(1),kndex,1)
      call sspoints('XR',xr,fxr(2),kndex+1,1)
      call sspoints('XR',xr,fxr(3),kndex+2,1)
      call sspoints('XR',xr,fxr(4),kndex+3,1)
c      FXR(1)=XR(KNDEX)
c      FXR(2)=XR(KNDEX+1)
c      FXR(3)=XR(KNDEX+2)
c      FXR(4)=XR(KNDEX+3)
C
      KUD=2
      IF(IUD.EQ.2) KUD=1
      IFLGG=0
      NXYI1=0
C
      CALL iBOOLE(FXG,FXR,16000,KGR,TGR,NEXT,IFORK,150,IER)
C
      IF(KLUE1.GT.6.OR.KLUE1.LE.0) GO TO 100
C
C  PROCESS RESULT CORRECTLY
C
      GO TO (100,140,190,190,150),KLUE1
 140  IC=0
      KITS=KITS+1
      DO 145 J=1,NRR
      IC=IC+1
C      CALL PUTRX(IOFX+IC,P(J+IOFP))
C      CALL PUTRY(IOFY+IC,Q(J+IOFQ))
       call sspoints('P ',p,pj,(j+iofp),1)
       call sspoints('Q ',q,qj,(j+iofq),1)
c      x(iofx+ic)=p(j+iofp)
c      y(iofy+ic)=q(j+iofq)
       call sspoints('X ',x,pj,(iofx+ic),2)
       call sspoints('Y ',y,qj,(iofy+ic),2)
 145  CONTINUE
      NGG=NRR
      FXG(1)=FXR(1)
      FXG(2)=FXR(2)
      FXG(3)=FXR(3)
      FXG(4)=FXR(4)
      GO TO 100
 150  IC=0
      KITS=KITS+1
      NGG=NXYI1
      DO 155 I=1,NGG
      IC=IC+1
C      CALL PUTRX(IC+IOFX,ABS(XY(1,IC)))
C      CALL PUTRY(IC+IOFY,XY(2,IC))
      call sspoints('X1',xy1,xy1ic,ic,1)
      call sspoints('X2',xy2,xy2ic,ic,1)
c      x(ic+iofx)=abs(xy1(ic))
c      y(ic+iofy)=xy2(ic)
      xy1icc=abs(xy1ic)
      call sspoints('X ',x,xy1icc,(ic+iofx),2)
      call sspoints('Y ',y,xy2ic,(ic+iofy),2)
      IF(XY1IC.LT.0.0) GO TO 157
      FXG(1)=AMIN1(FXG(1),ABS(XY1IC))
      FXG(2)=AMAX1(FXG(2),ABS(XY2IC))
      FXG(3)=AMIN1(FXG(3),XY2IC)
      FXG(4)=AMAX1(FXG(4),XY2IC)
 155  CONTINUE
 157  NGG=IC
      IC=ISL1(II)
      IF(IC.GT.0) IDS(IC)=-IDS(IC)
       ISL1(II)=-ISL1(II)
 190  ISL2(JJ)=-ISL2(JJ)
 100  CONTINUE
C
      IF(KITS.GT.1) write(nprnt,*) ' KITTY CAT GREATER THAN 1 '
      IF(KITS.GT.1) GO TO 200
      IF(KITS.EQ.0) GO TO 200
      NMIN=NMIN+1
      ISL3(NMIN)=IOFP+1
      IC=0
      KL=NGG+1
C
C  COMPUTE NEW MBR FOR NEW ISLAND AND SHUFFLE NEW ISLAND FROM RESULT
C  ARRAY INTO P,Q ARRAYS
C
      FXR(1)=9999999.
      FXR(2)=-FXR(1)
      FXR(3)=FXR(1)
      FXR(4)=FXR(2)
      DO 210 I=1,NGG
      IC=IC+1
      KL=KL-1
      call sspoints('X ',x,xt,(iofx+ic),1)
      call sspoints('Y ',y,yt,(iofy+ic),1)
c      XT=X(IOFX+IC)
c      YT=Y(IOFY+IC)
C      CALL PUTRP(IOFP+KL,XT)
C      CALL PUTRQ(IOFQ+KL,YT)
c      p(iofp+kl)=xt
c      q(iofq+kl)=yt
      call sspoints('P ',p,xt,(iofp+kl),2)
      call sspoints('Q ',q,yt,(iofq+kl),2)
      FXR(1)=AMIN1(XT,FXR(1))
      FXR(2)=AMAX1(XT,FXR(2))
      FXR(3)=AMIN1(YT,FXR(3))
      FXR(4)=AMAX1(YT,FXR(4))
 210  CONTINUE
      IOFP=IOFP+NGG
      IOFQ=IOFP
C
C  STORE NEW MBR IN ISLAND MBR STACK (MINOR POLYGON)
C
       KNDEX=1419+(JH+NMIN-1)*4+1
C      CALL PTRXR(KNDEX,FXR(1))
C      CALL PTRXR(KNDEX+1,FXR(2))
C      CALL PTRXR(KNDEX+2,FXR(3))
C      CALL PTRXR(KNDEX+3,FXR(4))
      call sspoints('XR',xr,fxr(1),kndex,2)
c      xr(kndex)=fxr(1)
      call sspoints('XR',xr,fxr(2),kndex+1,2)
c      xr(kndex+1)=fxr(2)
      call sspoints('XR',xr,fxr(3),kndex+2,2)
c      xr(kndex+2)=fxr(3)
      call sspoints('XR',xr,fxr(4),kndex+3,2)
c      xr(kndex+3)=fxr(4)
      IF(ISL1(II).GT.0) ISL1(II)=-ISL1(II)
      GO TO 200
C
 400  CONTINUE
C
C  UPDATE ISLAND POINTERS
C
      IF(NMIN.EQ.0) IFLAG=1
      IF(NMIN.EQ.0) RETURN
      KOUNT=0
C
      DO 300 I=1,NMIN
C
      JH=JH+1
      JDS(JH)=ISL3(I)
      MIN=MIN+1
      ISL2(MIN)=JH
 300  CONTINUE
C
C
C  CONFIGURE JDS PROPERLY
C
      LOOP=MIN-NMIN
C
      DO 310 I=1,LOOP
C
                INDEX=ISL2(I)
        IF(INDEX.GT.0) GO TO 310
C
        INDEX=IABS(INDEX)
        JDS(INDEX)=-JDS(INDEX)
C
  310 CONTINUE
      JDS(JH+1)=IOFP
C
      RETURN
      END
