      SUBROUTINE FIGURE
      COMMON /BOPRMS/ IUD, NPTS1,NPTS2,NXYI,NLOBE,KLUE,NLIST,IFLGG
      COMMON /IPPY/ IPP(7)
$Include:'io.inc'
      COMMON /DOT/ IH,JH,IDS(353),JDS(353)
      COMMON /VALS/ TGR(2,200)
$Include:'xypnts.500'
$Include:'pqpnts.500'
$Include:'rspnts.500'
$Include:'xyxy.500'
C
C
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
CC                S U B R O U T I N E   F I G U R E
C
C  FUNCTION:  FIGURE OUT WHAT TO DO AFTER OVERLAYING MAJOR AND MINOR
C             POLYGON BOUNDARIES.
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
      IPP(4)=NPTS1
      IPP(5)=NPTS1
      IPP(6)=NPTS2
      IPP(7)=NPTS2
      IF(KLUE.LT.0.OR.KLUE.GT.5) GO TO 999
C
      GO TO(100,200,300,400,500),KLUE
 100  CONTINUE
C
C  HANDLE SPECIAL CASES OF OR/NOT
C
      IF(IUD.EQ.1) RETURN
C      IF(IUD.EQ.3) CALL STOREP(NPTS1,IDS,IH,1,IUD)
C
C  MAJOR IN MINOR
C
 200  CONTINUE
      CALL MAJMIN(JUMP,IER)
      IF(IER.NE.0) RETURN
      IF(JUMP.EQ.0) RETURN
      IF(JUMP.EQ.1) GO TO 510
C
C MINOR IN MAJOR
C
 300  CONTINUE
      IF(IH.GT.0) GO TO 310
      IF(IUD.EQ.2) GO TO 200
      NPTS=NPTS2
      IF(JH.GT.0) NPTS=JDS(JH+1)
C      CALL STOREP(NPTS,JDS,JH,2,IUD)
       call inout1(npts,jds,jh,2)
      RETURN
 310  CONTINUE
      DO 315 I=1,NPTS2
      call sspoints('P ',p,xy1i,i,1)
      call sspoints('Q ',q,xy2i,i,1)
c      xy1(i)=p(i)
c      xy2(i)=q(i)
      call sspoints('X1',xy1,xy1i,i,2)
      call sspoints('X2',xy2,xy2i,i,2)
 315  CONTINUE
      NXYI=NPTS2
      NLOBE=1
      call sspoints('X1',xy1,xt,npts2,1)
C      XT=-XY1(NPTS2)
C      xy(1,npts2)=xt
      xt=(-xt)
      call sspoints('X1',xy1,xt,npts2,2)
      GO TO 510
C
C  MAJOR=MINOR
C
 400  CONTINUE
      NPTS=NPTS2
      IF(JH.GT.0) NPTS=JDS(JH+1)
C      CALL STOREP(NPTS,JDS,JH,2,IUD)
       call inout1(npts,jds,jh,2)
      RETURN
C
C  NORMAL
C
 500  CONTINUE
      IF(IUD.LT.3.AND.(JH.GT.0.OR.IH.GT.0)) GO TO 510
      IF(IUD.LT.3) GO TO 502
C
C .NOT. BUT MINOR IS HOLE IN MAJOR
C
      IF(NLOBE.GT.0) GO TO 502
      NPTS=0
C
      IH=IH+1
      NPTS=IDS(IH)
      IDS(IH)=NPTS+1
      K=NPTS+NPTS2+1
      IDS(IH+1)=K-1
C
      DO 503 I=1,NPTS2
      NPTS=NPTS+1
      K=K-1
c                x(npts)=p(k)
      call sspoints('P ',p,pk,k,1)
      call sspoints('Q ',q,qk,k,1)
c                y(npts)=q(k)
      call sspoints('X ',x,pk,npts,2)
      call sspoints('Y ',y,qk,npts,2)
  503 CONTINUE
C      CALL STOREP(NPTS,IDS,IH,1,IUD)
       call inout1(npts,ids,ih,1)
      RETURN
C
 502  CONTINUE
      IC=0
      DO 505 I=1,NLOBE
      NPTS=0
 504  CONTINUE
      NPTS=NPTS+1
      IC=IC+1
      call sspoints('X1',xy1,xt,ic,1)
      call sspoints('X2',xy2,xt2,ic,1)
      xtt=abs(xt)
c      XT=ABS(XY1(IC))
      call sspoints('R ',r,xtt,npts,2)
      call sspoints('S ',s,xt2,npts,2)
c      r(npts)=xt
c      s(npts)=xy(2,ic)
      IF(xt.GT.0.0) GO TO 504
C
C  MOVE ISLANDS (ONLY IF NLOBE EQ 1)
C
       IF(NLOBE.GT.1) GO TO 509
      IF(IH.EQ.0) GO TO 509
C
         ISTAR=IDS(1)
         ISTOP=IDS(IH+1)
         IDIFF=NPTS-IDS(1)
C
         DO 508 J=1,IH
C
           IDS(J)=IDS(J)+IDIFF
C
  508    CONTINUE
C
         DO 507 J=ISTAR,ISTOP
C
           NPTS=NPTS+1
           call sspoints('X ',x,xj,j,1)
           call sspoints('Y ',y,yj,j,1)
c           r(npts)=x(j)
c           s(npts)=y(j)
            call sspoints('R ',r,xj,npts,2)
            call sspoints('S ',s,yj,npts,2)
C
  507    CONTINUE
C
         IDS(IH+1)=NPTS
C
 509   CONTINUE
C       CALL STOREP(NPTS,IDS,IH,3,IUD)
        call inout1(npts,ids,ih,3)
 505  CONTINUE
      RETURN
C
C  UNION WITH ISLANDS (BUT NO ISLANDS IN INPUT DATA)
C
 506  CONTINUE
 510  CONTINUE
      CALL PISL
      DO 998 I=1,353
       IDS(I)=IABS(IDS(I))
       JDS(I)=IABS(JDS(I))
 998   CONTINUE
 999   RETURN
      END
