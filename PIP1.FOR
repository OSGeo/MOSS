      SUBROUTINE PIP1(PP,QQ,IND,N,IH,IDS)
C
C *****************************************************************************
C
C             S U B R O U T I N E     P I P 1
C FUNCTION:
C             THIS ROUTINE DETERMINES IF THE POINT, (PP,QQ),
C             IS IN THE POLYGON.
C PARAMETERS:
C             (PP,QQ)   IN  - THE X AND Y LOCATION OF POINT.
C             IND       OUT - 0,IF POINT OUTSIDE POLYGON,
C                             1,IF SOMETHING ELSE.
C             N         IN  - THE NUMBER OF POINTS.
C             IH        IN  - THE NUMBER OF ISLAND POINTERS IN
C                             THE IDS ARRAY.
C             IDS       IN  - THE ISLAND ARRAY.
C ROUTINES
C *****************************************************************************
C
$Include:'xypnts.inc'
$Include:'pqpnts.inc'
      DOUBLE PRECISION QQ,PP,X1,Y1,X2,Y2,YREM,YLAST,
     1 XTEMP,RSLXY
      DIMENSION IDS(1)
      LOGICAL SWT
      SWT=.FALSE.
      M=N-1
      YREM=-1.D0
      IND=0
      DO 10 I=1,M
      IF(IH.EQ.1)GO TO 9
      DO 8 II=1,IH
      IF(IDS(II).EQ.I)GO TO 10
    8 CONTINUE
    9 CONTINUE
      call spoints('Q ',q,qi,i,1)
      call spoints('Q ',q,qi1,i+1,1)
      call spoints('P ',p,pi,i,1)
      call spoints('P ',p,pi1,i+1,1)
      Y1=dble(QI)
      Y2=dble(QI1)
      X1=dble(PI)
      X2=dble(PI1)
      IF(QQ.GT.Y1.AND.QQ.GT.Y2)GO TO 77
      IF(QQ.LT.Y1.AND.QQ.LT.Y2)GO TO 77
      IF(PP.GT.X1.AND.PP.GT.X2)GO TO 77
      IF(QQ.NE.Y1)GO TO 20
      IF(PP.EQ.X1)GO TO 30
      GO TO 77
   20 IF(QQ.NE.Y2)GO TO 80
      IF(PP.EQ.X2)GO TO 30
      IF(QQ.EQ.YREM)GO TO 40
      YREM=Y2
      LAST=I+1
90    CONTINUE
      JUMP=1
      GO TO 200
91    CONTINUE
      call spoints('Q ',q,qlastt,last,1)
      IF(QQ.EQ.DBLE(QLASTT))GO TO 90
      YLAST=QLASTT
      NEXT=I+1
100   CONTINUE
      JUMP=2
      GO TO 300
101   CONTINUE
      call spoints('Q ',q,qnextt,next,1)
      IF(QQ.EQ.DBLE(QNEXTT))GO TO 100
      YNEXT=QNEXTT
      IF(QQ.GT.YLAST.AND.QQ.GT.YNEXT)GO TO 10
      IF(QQ.LT.YLAST.AND.QQ.LT.YNEXT)GO TO 10
      IF(PP.LT.X2)GO TO 70
      GO TO 10
C
C                  DECREMENT LAST
200   CONTINUE
      LAST=LAST-1
      L=IH-1
      IF(L.EQ.0) L=1
      DO 210 K=1,L
      IF(LAST.NE.IDS(K)) GO TO 210
      LAST=IDS(K+1)-1
      IF(JUMP-1) 91,91,101
210   CONTINUE
C
      IF(JUMP-1) 91,91,101
C
C
C
C                  INCREMENT NEXT
300   CONTINUE
      L=IH
      IF(L.EQ.1) L=2
      DO 310 K=2,L
      IF(NEXT.NE.IDS(K)) GO TO 310
      NEXT=IDS(K-1)+2
      IF(JUMP-1) 91,91,101
310   CONTINUE
C
      NEXT=NEXT+1
      IF(JUMP-1) 91,91,101
C
C
   40 IF(Y1.NE.Y2)YREM=-1.D0
      GO TO 10
   80 YREM=-1.D0
      IF(PP.LE.X1.AND.PP.LE.X2)GO TO 70
      IF(X1.EQ.X2)GO TO 30
      IF(Y1.EQ.Y2)GO TO 30
      RSLXY=(X1-X2)/(Y1-Y2)
      XTEMP=RSLXY*(QQ-Y2)+X2
      IF(PP.GT.XTEMP-5.D-5.AND.
     1   PP.LT.XTEMP+5.D-5) GO TO 30
      IF(RSLXY.GT.5.D+6) GO TO 30
      IF(RSLXY.LT.-5.D+6) GO TO 30
      IF(PP.GT.XTEMP)GO TO 10
   70 SWT=.NOT.SWT
      GO TO 10
   30 SWT=.TRUE.
      IND=-2
      GO TO 5000
   77 YREM=-1.D0
   10 CONTINUE
C
      IF(SWT)IND=1
5000  RETURN
      END
