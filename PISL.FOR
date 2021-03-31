      SUBROUTINE PISL
      COMMON /BOPRMS/ KUD,NGG,NRR,NXYI1,KLOBE,KLUE1,NLIST,IFLGG
      COMMON /PISTF/ INDEX,KNDEX,ICL,FXR(4),NXYI,NLOBE,
     1 KP,KP2,MAJ,MIN,II,I,J,KH,ISTAR,ISTOP,NGT,NRT,IUD
$Include:'work.inc'
      COMMON /KTABL/ ISL1(200),ISL2(200)
$Include:'io.inc'
      COMMON /VALS/ TGR(2,200)
      COMMON /VALS1/ KGR(2,200),NEXT(2,200),IFORK(200)
      COMMON /DOT/ IH,JH,IDS(353),JDS(353)
      COMMON /IPPY/ IPP(7)
      COMMON /PISLL/ ibuff2(2,200),WINDS(100)
$Include:'xypnts.500'
$Include:'pqpnts.500'
$Include:'rspnts.500'
$Include:'xrxr.500'
$Include:'xyxy.500'
      EQUIVALENCE (IPP(4),IOFX),(IPP(5),IOFY)
       EQUIVALENCE (IPP(6),IOFP),(IPP(7),IOFQ)
C
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
C
C           S U B R O U T I N E   P I S L
C
C  ISLAND PROCESSING FOR POLYGON OVERLAY
C  IPP(1)  =  MBR'S MAJOR POLYGON = 1
C  IPP(2)  =  MBR'S MINOR POLYGON = 1420
C  IPP(3)  =  MBR'S RESULTING POLYGONS = 2840
C
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
C  DETERMINE WHAT ISLANDS (IF ANY) MAY INTERSECT THE RESULTING
C  POLYGON ? ?
C
      NLOBE=KLOBE
      IUD=KUD
      NGT=NGG
      NRT=NRR
      NXYI=NXYI1
      IPP(4)=IDS(IH+1)
      IPP(5)=IPP(4)
      IPP(6)=JDS(JH+1)
      IPP(7)=IPP(6)
  1    CONTINUE
       NRES=1
      IBUFF(NRES)=1
      IPP3=0
      INDEX=2839
      IC=0
  10  CONTINUE
      XMIN=9999999.
      YMIN=XMIN
      XMAX=-XMIN
      YMAX=XMAX
  5   CONTINUE
      IC=IC+1
      IF(IC.GT.NXYI) GO TO 20
      call sspoints('X1',xy1,xy1ic,ic,1)
      XMIN=AMIN1(ABS(XY1IC),XMIN)
      call sspoints('X2',xy2,xy2ic,ic,1)
      YMIN=AMIN1(XY2IC,YMIN)
      XMAX=AMAX1(ABS(XY1IC),XMAX)
      YMAX=AMAX1(XY2IC,YMAX)
      INDEX=INDEX+1
        xy1icc=abs(xy1ic)
        call sspoints('XR',xr,xy1icc,index,2)
C                         xr(index)=abs(xy1(ic))
       INDEX=INDEX+1
        call sspoints('XR',xr,xy2ic,index,2)
C                         xr(index)=xy(2,ic)
      IF(XY1IC.GT.0.0) GO TO 5
      NRES=NRES+1
      IBUFF(NRES)=IC+1
      IPP3=IPP3+1
      WINDS(IPP3)=XMIN
      IPP3=IPP3+1
      WINDS(IPP3)=XMAX
      IPP3=IPP3+1
      WINDS(IPP3)=YMIN
      IPP3=IPP3+1
      WINDS(IPP3)=YMAX
      GO TO 10
C
  20  CONTINUE
      if(nres.eq.1) then
        NRES=NRES+1
       IBUFF(NRES)=IC+1
       IPP3=IPP3+1
       WINDS(IPP3)=XMIN
       IPP3=IPP3+1
       WINDS(IPP3)=XMAX
       IPP3=IPP3+1
       WINDS(IPP3)=YMIN
       IPP3=IPP3+1
       WINDS(IPP3)=YMAX
      endif
      NRES=NRES-1
C
      II=0
  200  CONTINUE
       II=II+1
      MAJ=0
      MIN=0
      IF(IH.EQ.0) GO TO 35
      KP=1
      KP2=(II-1)*4+1
      DO 30 I=1,IH
      IF(IDS(I).LT.0) GO TO 29
      call sspoints('XR',xr,xrkp,kp,1)
      IF(XRKP.GT.WINDS(KP2+1)) GO TO 29
      call sspoints('XR',xr,xrkp1,kp+1,1) 
      IF(XRKP1.LT.WINDS(KP2)) GO TO 29
      call sspoints('XR',xr,xrkp2,kp+2,1)
      IF(XRKP2.GT.WINDS(KP2+3)) GO TO 29
      call sspoints('XR',xr,xrkp3,kp+3,1)
      IF(XRKP3.LT.WINDS(KP2+2)) GO TO 29
      MAJ=MAJ+1
      ISL1(MAJ)=I
  29  KP=KP+4
  30  CONTINUE
C
  35   CONTINUE
         IF(JH.EQ.0) GO TO 45
      KP=1420
      KP2=(II-1)*4+1
      DO 40 I=1,JH
      IF(JDS(I).LT.0) GO TO 39
      call sspoints('XR',xr,xrkp,kp,1)
      IF(XRKP.GT.WINDS(KP2+1)) GO TO 39
      call sspoints('XR',xr,xrkp1,kp+1,1)
      IF(XRKP1.LT.WINDS(KP2)) GO TO 39
      call sspoints('XR',xr,xrkp2,kp+2,1)
      IF(XRKP2.GT.WINDS(KP2+3)) GO TO 39
      call sspoints('XR',xr,xrkp3,kp+3,1)
      IF(XRKP3.LT.WINDS(KP2+2)) GO TO 39
      MIN=MIN+1
      ISL2(MIN)=I
  39  KP=KP+4
  40  CONTINUE
C
  45  CONTINUE
C
C  WE KNOW WHAT ISLANDS HAVE TO BE PROCESSED (IF ANY)
C
      IF(MAJ.GT.0.OR.MIN.GT.0) GO TO 50
C
      CALL MOVXR(IBUFF,II,NGG,IOFX,2)
      KH=0
C      CALL STOREP(NGG,JDS,KH,3,IUD)
       call inout1(ngg,jds,kh,3)
      IF(II.EQ.NLOBE) GO TO 300
      IBUFF(II)=-IBUFF(II)
      GO TO 200
C
  50  CONTINUE
C
C  REALLY SOMETHING.  MOVE XY TO TEMPORARY STORAGE
C
       IF(MAJ.EQ.0.OR.MIN.EQ.0) GO TO 100
C
C  TAKE UNION OF ISLANDS
C
      CALL POSL(ISL1,ISL2,IFLAG)
C
C  TAKE NOT OF ISLANDS WITH RESULT POLYGONS
C
  100  CONTINUE
      CALL MOVXR(IBUFF,II,NGG,IOFX,1)
      ICL=0
      IF(MAJ.EQ.0) GO TO 150
C
C  MOVE MAJOR ISLAND TO P,Q
C
       DO 110 I=1,MAJ
       IPASSES=0
       IPSIGN=-1
       INDEX=ISL1(I)
      IF(INDEX.LT.0) GO TO 110
       ISTAR=IDS(INDEX)
      ISTOP=IABS(IDS(INDEX+1))-1
       IF(INDEX.EQ.IH) ISTOP=ISTOP+1
      KNDEX=(INDEX-1)*4+1
       call sspoints('XR',xr,fxr(1),kndex,1)
       call sspoints('XR',xr,fxr(2),kndex+1,1)
       call sspoints('XR',xr,fxr(3),kndex+2,1)
       call sspoints('XR',xr,fxr(4),kndex+3,1)
c       FXR(1)=XR(KNDEX)
c       FXR(2)=XR(KNDEX+1)
c       FXR(3)=XR(KNDEX+2)
c       FXR(4)=XR(KNDEX+3)
       IC=0
C
       DO 105 J=ISTAR,ISTOP
      IC=IC+1
       call sspoints('X ',x,xtttt,j,1)
C      XTTTT=X(J)
        call sspoints('P ',p,xtttt,(iofp+ic),2)
C        p(iofp+ic)=xtttt
        call sspoints('Y ',y,xtttt,j,1)
C      XTTTT=Y(J)
        call sspoints('Q ',q,xtttt,(iofq+ic),2)
C        q(iofq+ic)=xtttt
 105  CONTINUE
      NRR=ISTOP-ISTAR+1
 106   CONTINUE
       KUD=3
      IFLGG=1
      NXYI1=0
      CALL iBOOLE(WINDS(KP2),FXR,16000,
     1             KGR,TGR,NEXT,IFORK,200,KERR)
      IF(KERR.EQ.3) CALL POSHFT(IPASSES,IPSIGN,I,iofp+1,(iofp+nrr),I)
      IF(KERR.EQ.3) GO TO 106
      IF(KLUE1.NE.5) GO TO 94
      IF(KLOBE.GE.2) GO TO 93
      IF(KLOBE.EQ.1) GO TO 95
C
C  ISLAND COMPLETELY IN PARENT
C
      ICL=ICL+1
       ibuff2(1,ICL)=-ISTAR
       ibuff2(2,ICL)=ISTOP
       GO TO 110
  93  CONTINUE
        IDS(INDEX)=-IDS(INDEX)
        GO TO 500
  94    IDS(INDEX)=-IDS(INDEX)
      IF(KLUE1.EQ.1) GO TO 230
      IF(KLUE1.EQ.2) IDS(INDEX)=IABS(IDS(INDEX))
        GO TO 110
C
C  ISLAND LOPED OFF PARENT
C
  95   CONTINUE
      NGG=NXYI1
      IC=0
       DO 96 J=1,NGG
       IC=IC+1
       call sspoints('X1',xy1,xtttt,j,1)
C      XTTTT=ABS(XY1(J))
       xtttt=abs(xtttt)
         call sspoints('X ',x,xtttt,(iofx+ic),2)
C        x(iofx+ic)=xtttt
        call sspoints('X2',xy2,xtttt,j,1)
C                             XTTTT=XY2(J)
Cccccc       CALL PUTRY(IOFX+IC,XTTTT)
        call sspoints('Y ',y,xtttt,(iofx+ic),2)
C       y(iofx+ic)=xtttt
  96   CONTINUE
      GO TO 94
  110  CONTINUE
C
C  PROCESS MINOR ISLANDS
C
 150   CONTINUE
       IF(MIN.EQ.0) GO TO 190
       DO 120 I=1,MIN
       IPASSES=0
       IPSIGN=-1
       INDEX=ISL2(I)
      IF(INDEX.LT.0) GO TO 120
       IOFP=JDS(INDEX)-1
       IOFQ=IOFP
      ISTAR=IABS(JDS(INDEX))
      ISTOP=IABS(JDS(INDEX+1))-1
      IF(INDEX.EQ.JH) ISTOP=ISTOP+1
      NRR=ISTOP-ISTAR+1
 151  CONTINUE
      KUD=3
      KNDEX=(INDEX-1)*4+1
      KNDEX=KNDEX+1419
      call sspoints('XR',xr,fxr(1),kndex,1)
      call sspoints('XR',xr,fxr(2),kndex+1,1)
      call sspoints('XR',xr,fxr(3),kndex+2,1)
      call sspoints('XR',xr,fxr(4),kndex+3,1)
c      FXR(1)=XR(KNDEX)
c      FXR(2)=XR(KNDEX+1)
c      FXR(3)=XR(KNDEX+2)
c      FXR(4)=XR(KNDEX+3)
        KP4=KP2+3
      IFLGG=1
      NXYI1=0
      CALL iBOOLE(WINDS(KP2),FXR,16000,
     1  KGR,TGR,NEXT,IFORK,200,KERR)
      IF(KERR.EQ.3) CALL POSHFT(IPASSES,IPSIGN,I,istar,istop,I)
      IF(KERR.EQ.3) GO TO 151
      IF(KLUE1.NE.5) GO TO 124
      IF(KLOBE.GE.2) GO TO 123
       IF(KLOBE.EQ.1) GO TO 115
C
C  ISLAND COMPLETELY IN PARENT
C
       ICL=ICL+1
       ibuff2(1,ICL)=IOFP+1
       ibuff2(2,ICL)=IOFP+NRR
       GO TO 120
 123   JDS(INDEX)=-JDS(INDEX)
       GO TO 500
 124   JDS(INDEX)=-JDS(INDEX)
      IF(KLUE1.EQ.1) GO TO 230
      IF(KLUE1.EQ.2) JDS(INDEX)=IABS(JDS(INDEX))
       GO TO 120
C
C  ISLAND LOPPED OFF PARENT
C
 115   CONTINUE
       NGG=NXYI1
       IC=0
       DO 116 J=1,NGG
       IC=IC+1
       call sspoints('X1',xy1,xtttt,j,1) 
c      XTTTT=ABS(XY1(J))
       xtttt=abs(xtttt)
       call sspoints('X ',x,xtttt,(iofx+ic),2)
C       x(iofx+ic)=xtttt
       call sspoints('X2',xy2,xtttt,j,1)
C      XTTTT=XY2(J)
       call sspoints('Y ',y,xtttt,(iofx+ic),2)
C       y(iofx+ic)=xtttt
 116  CONTINUE
      GO TO 124
 120  CONTINUE
 190   CONTINUE
C
C  PROCESSING OF THIS LOBE COMPLETE. MOVE IT TO R/S
C
      DO 210 I=1,NGG
C      CALL PUTRR(I,X(I+IOFX))
C       CALL PUTRS(I,Y(I+IOFY))
        call sspoints('X ',x,ri,(i+iofx),1)
        call sspoints('Y ',y,si,(i+iofy),1)
c                        r(i)=x(i+iofx)
c                        s(i)=y(i+iofy)
        call sspoints('R ',r,ri,i,2)
        call sspoints('S ',s,si,i,2)
 210  CONTINUE
      IF(ICL.EQ.0) GO TO 220
      DO 205 I=1,ICL
      ISTAR=ibuff2(1,I)
      ISTOP=ibuff2(2,I)
      IF(ISTAR.LT.0) GO TO 202
C
      IFORK(I)=NGG+1
      DO 201 J=ISTAR,ISTOP
      NGG=NGG+1
C      CALL PUTRR(NGG,P(J))
C      CALL PUTRS(NGG,Q(J))
       call sspoints('P ',p,pj,j,1)
       call sspoints('Q ',q,qj,j,1)
c       r(ngg)=p(j)
c       s(ngg)=q(j)
       call sspoints('R ',r,pj,ngg,2)
       call sspoints('S ',s,qj,ngg,2)
 201  CONTINUE
      GO TO 205
 202  CONTINUE
      IFORK(I)=NGG+1
      ISTAR=IABS(ISTAR)
      DO 203 J=ISTAR,ISTOP
      NGG=NGG+1
C      CALL PUTRR(NGG,X(J))
C      CALL PUTRS(NGG,Y(J))
       call sspoints('X ',x,xj,j,1)
       call sspoints('Y ',y,yj,j,1)
c      r(ngg)=x(j)
c      s(ngg)=y(j)
       call sspoints('R ',r,xj,ngg,2)
       call sspoints('S ',s,yj,ngg,2)
 203  CONTINUE
 205  CONTINUE
 220  CONTINUE
C      CALL STOREP(NGG,IFORK,ICL,3,IUD)
       call inout1(ngg,ifork,icl,3)
  230   CONTINUE
       IF(II.EQ.NLOBE) GO TO 300
       IBUFF(II)=-IBUFF(II)
       GO TO 200
 300   CONTINUE
C
       IOFX=0
       IOFY=0
       IOFP=0
       IOFQ=0
      NGG=NGT
      NRR=NRT
      KLUE1=0
      KUD=IUD
      RETURN
C
C  ISLAND SPLIT PARENT.  RE-PROCESS ALL DATA
C
 500  CONTINUE
      NXYI=NXYI1
      IC=0
      DO 510 I=1,NLOBE
      IF(I.EQ.II) GO TO 510
      IF(IBUFF(I).LT.0) GO TO 510
      CALL MOVXR(IBUFF,I,NXYI,IOFX,3)
C      CALL PUTXY(1,NXYI,-XY(1,NXYI))
       call sspoints('X1',xy1,xy1nxyi,nxyi,1)
       xy1nxyi=-xy1nxyi
       call sspoints('X1',xy1,xy1nxyi,nxyi,2)
C       xy(1,nxyi)=-xy(1,nxyi)
      IC=IC+1
 510  CONTINUE
      NLOBE=IC+KLOBE
      GO TO 1
      END
