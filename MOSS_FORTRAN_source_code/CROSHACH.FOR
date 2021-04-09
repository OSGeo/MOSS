C
C  THE FUNCTION OF THIS ROUTINE IS TO SHADE A POLYGON
C
C  INPUT PARAMETERS
C
C        ANGLE   = DEGREE OF ROTATION FOR  CROSS HATCHING
C        D    = DISTANCE BETWEEN SHADING LINES
C
        SUBROUTINE croshach(N,ANGLE,D,NSL,ISLAND,LTY,
     +                    WIDTHL,DMARK,IPEN1,IPEN2)
C
$Include:'xypnts.inc'
$Include:'rspnts.inc'
        COMMON /DIMEN/ VX1,VX2,VY1,VY2,XLN,YLN,SCALE,FACT,X0,Y0
        DIMENSION XH(2),YH(2),XR(3000),PP(4),ISLAND(1)
        REAL LO
C
10      IF(N.LE.32700)GO TO 20
        WRITE(*,*)
     +  'In Croshach.FoR- # of pnts>32700, POLYGON IGONORED!'
        RETURN
C
 20     THETA=ANGLE*0.0174533
        C=COS(THETA)
        SS=SIN(THETA)
        CALL CPTRNS(VX1,WX1,VX1,SCALE)
C/*  CHANGE TO PLOTTER INCHES
        CALL CPTRNS(VX2,WX2,VX1,SCALE)
        CALL CPTRNS(VY1,WY1,VY1,SCALE)
        CALL CPTRNS(VY2,WY2,VY1,SCALE)
        PP(1)=-WX1*SS + WY1*C
        PP(2)=-WX1*SS + WY2*C
        PP(3)=-WX2*SS + WY2*C
        PP(4)=-WX2*SS + WY1*C
        PLO=PP(1)
        PHI=PP(1)
C
        DO 30 I=2,4
        IF(PP(I).LT.PLO) PLO=PP(I)
        IF(PP(I).GT.PHI) PHI=PP(I)
 30     CONTINUE
C
        NPTS=N
        IF(NSL.GT.0) NPTS=ISLAND(1)-1
C/*  # PTS IN PRIMARY POLYGON
        LO=9.E20
        HI=-LO
        DO 40 I=1,N
        call spoints('X ',x,xttr,i,1)
        CALL CPTRNS(xttr,XI,VX1,SCALE)
C/*  CONVERT TO PLOTTER INCHES
        call spoints('Y ',y,yttr,i,1)
        CALL CPTRNS(yttr,YI,VY1,SCALE)
C/*  CONVERT TO PLOTTER INCHES
        RT=XI*C+YI*SS
         call spoints('R ',r,rt,i,2)
c        r(i)=rt
        P=-XI*SS+YI*C
         call spoints('S ',s,p,i,2)
c        s(i)=p 
        IF(I.GT.NPTS) GO TO 40
C/*  MIN/MAX NOT WITHIN AN ISLAND
        IF(P.LT.PLO) P=PLO
        IF(P.GT.PHI) P=PHI
        IF(P.LT.LO) LO=P
        IF(P.GT.HI) HI=P
 40     CONTINUE
        P=PLO-D/2.
        I=0
 50     I=I+1
        P=P+D
        IF(P.LT.LO) GO TO 50
C/*  SHADE LINE DOES NOT CROSS POLYGON
        IF(P.GT.HI) GO TO 140
C/*  SHADE LINE BEYOND POLYGON
        M=0
        IPOINT=1
        ISLAND(NSL+1)=0
        call spoints('S ',s,sj,1,1)
c        SJ=S(1)
        DO 70 L=1,N-1
        J=L+1
        SL=SJ
        call spoints('S ',s,sj,j,1)
c        SJ=S(J)
        IF(ISLAND(IPOINT).NE.J)GO TO 60
        IPOINT=IPOINT+1
        GO TO 70
C/* ISLAND OF POLYGON
 60     IF(SL.LE.P .AND. SJ.LE.P) GO TO 70
        IF(SL.GT.P .AND. SJ.GT.P) GO TO 70
        M=M+1
        IF(M.GT.3000) GO TO 900
C/*  EXCEEDED DIMENSION
        call spoints('R ',r,rttl,l,1)
        call spoints('R ',r,rttj,j,1)
        XR(M)=Rttl+(Rttj-Rttl)*(P-SL)/(SJ-SL)
  70   CONTINUE
C
        IF(M.EQ.0) GO TO 50
 80     IFLAG=0
C/*  SORT IN ASCENDING ORDER
        DO 90 L=2,M
        IF(XR(L-1).LE.XR(L)) GO TO 90
        IFLAG=1
        Q=XR(L)
        XR(L)=XR(L-1)
        XR(L-1)=Q
90      CONTINUE
        IF(IFLAG.NE.0) GO TO 80
C/*  SORT AGAIN
        IF(LTY.LE.5 .AND. MOD(I,2).EQ.0) GO TO 100
        L=1
        NS=0
        GO TO 110
100     L=-1
        NS=M+1
110     CONTINUE
C
        DO 130 J=2,M,2
        DO 120 JJ=1,2
        NS=NS+L
        XH(JJ)=XR(NS)*C-P*SS
        YH(JJ)=XR(NS)*SS+P*C
  120  CONTINUE
        IF(LTY.GT.6) GO TO 125
        CALL COMDASH(XH(1),YH(1),3)
        IP=LTY+3
        IF(LTY.EQ.5) IP=2
        CALL COMDASH(XH(2),YH(2),IP)
        GO TO 130
 125    CALL BLMLINE(XH,YH,2,WIDTHL,DMARK,LTY,IPEN1,IPEN2)
  130  CONTINUE
        GO TO 50
 140    RETURN
C
 900    WRITE(*,*)
     +  'More than 3000 Intersections in CRSTCH - POLYGON IGNORED'
        RETURN
        END
