      SUBROUTINE CROSS(SSA,SSB,T,KROSS)
C      COMMON /CRTOL/ TOLER
$Include:'io.inc'
C
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
C WE ARE GIVEN TWO LINE SEGMENTS, SA AND SB.  WE DETERMINE WHETHER THEY
C INTERSECT AND REPORT THIS BY RETURNING KROSS.  THE POSITION OF THE
C POINT OF INTERSECTION IS RETURNED IN T.
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
       DOUBLE PRECISION IXA,IYA,IXB,IYB,IXAB,IYAB
      DOUBLE PRECISION SA(2,2),SB(2,2)
      REAL KOLLIN,MOXIE,MOX,IMOX,MOXB,MOXA
      INTEGER KROSS,I
      REAL SSA(2,2),SSB(2,2)
      LOGICAL PA,PB,PC,PD
      REAL T(4)
      data toler/.0001/
      KROSS=0
      IFLAG=0
      DO 2 J=1,2
      DO 2 I=1,2
      SA(I,J)=0.D0
      SB(I,J)=0.D0
      SA(I,J)=SSA(I,J)
    2 SB(I,J)=SSB(I,J)
      IXA=SA(1,2)-SA(1,1)
      IYA=SA(2,2)-SA(2,1)
      IXB=SB(1,2)-SB(1,1)
      IYB=SB(2,2)-SB(2,1)
      MOXIE=SNGL((IYA*IXB)-(IXA*IYB))
      IXAB=SB(1,1)-SA(1,1)
      IYAB=SB(2,1)-SA(2,1)
      IF(MOXIE.GT.-0.000001.AND.MOXIE.LE.0.000001) GO TO 10
C SEGMENTS ARE NOT PARALLEL.
      MOX=ABS(MOXIE)
      IMOX=SIGN(1.0,MOXIE)
      MOXA=SNGL(((IYAB*IXB)-(IXAB*IYB))*(IMOX))
      IF(MOXA.LT.-0.000001.OR.MOXA.GE.MOX) GO TO 100
      MOXB=SNGL((IYAB*IXA)-(IXAB*IYA))*(IMOX)
      IF(MOXB.LT.-0.000001.OR.MOXB.GE.MOX) GO TO 100
      BOT=MOX
      TA=(MOXA/BOT)
      TB=(MOXB/BOT)
      GO TO 90
C SEGMENTS ARE PARALLEL.
  10  KOLLIN=SNGL((IYAB*IXB)-(IXAB*IYB))
      KOLLIN=AINT(KOLLIN)
      IF(KOLLIN.NE.0.0) GO TO 100
C SEGMENTS ARE COLLINEAR
      IFLAG=1
      I=1
      IF(IXA.EQ.0.D0) I=2
      QA=SA(I,2)-SA(I,1)
      ta1=0.0
      ta2=0.0
      if(qa.gt.0.0)then
       TA1=(SB(I,1)-SA(I,1))/QA
       TA2=(SB(I,2)-SA(I,1))/QA
      endif
      QB=SB(I,2)-SB(I,1)
      tb1=0.0
      tb2=0.0
      if(qb.gt.0.0)then
        TB1=(SA(I,1)-SB(I,1))/QB
        TB2=(SA(I,2)-SB(I,1))/QB
      endif
      PA=TA1.GE.0.0.AND.TA1.LT.1.0
      PB=TB1.GE.0.0.AND.TB1.LT.1.0
      PC=TA2.GT.0.0.AND.TA2.LT.1.0
      PD=TB2.GT.00.AND.TB2.LT.1.0
      IF(.NOT.(PA.OR.PB.OR.PC.OR.PD)) GO TO 100
C THEY OVERLAP.
      IF(TA1.GT.TA2) GO TO 20
C THEY POINT IN THE SAME DIRECTION.
      TA=AMAX1(0.0,TA1)
      TB=AMAX1(0.0,TB1)
      GO TO 90
C THEY POINT IN OPPOSITE DIRECTIONS.
   20 IF(.NOT.(PA.OR.PB)) GO TO 100
      TA=0.0
      TB=0.0
      IF(PA.AND.PB.AND.TA1.LE.0.0) GO TO 90
      IF(PA) TA=TA1
      IF(PB) TB=TB1
      IF(.NOT.(PA.AND.PB)) GO TO 90
      KROSS=2
      T(1)=TA
      T(2)=0.
      T(3)=0.
      T(4)=TB
      XTT=SSA(1,1)+(SSA(1,2)-SSA(1,1))*TA
      YYT=SSA(2,1)+(SSA(2,2)-SSA(2,1))*TA
      CALL POISE(XXT,YYT,
     1  SSB(1,1),SSB(1,2),SSB(2,1),SSB(2,2),XINT,YINT,DSQ)
      IF(DSQ.GT.TOLER) KROSS=0
      GO TO 100
C
   90 KROSS=1
      T(1)=TA
      T(2)=TB
C
C  SPECIAL CASE OF PARALLEL, OPOSITE, BUT NOT COLLINEAR!!!!
C
      XXT=SSA(1,1)+(SSA(1,2)-SSA(1,1))*TA
      YYT=SSA(2,1)+(SSA(2,2)-SSA(2,1))*TA
      IF(IFLAG.EQ.1) CALL POISE(XXT,YYT
     1 ,SSB(1,1),SSB(1,2),SSB(2,1),SSB(2,2),XINT,YINT,DSQ)
      IF(IFLAG.EQ.1.AND.DSQ.GT.TOLER) KROSS=0
C RETURN
  100 CONTINUE
      RETURN
      END
