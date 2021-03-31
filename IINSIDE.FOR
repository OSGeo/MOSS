      SUBROUTINE iINSIDE(PP,NXY,IN,ISW)
      COMMON /IPPY/ IPPYY(3),IOFX,IOFY,IOFP,IOFQ
$Include:'xypnts.500'
$Include:'pqpnts.500'
C
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
C DETERMINES WHETHER A GIVEN POINT IN THE X-Y PLANE FALLS WITHIN A
C REGION DEFINED BY A POLYGON.
C
C THE X AND Y OF THE GIVEN POINT ARE IN P(1) AND P(2). THE VERTICES OF
C THE POLYGON ARE IN THE XY ARRAY, IN WHICH THE X AND Y OF THE J-TH
C VERTEX ARE IN XY(1,J) AND XY(2,J), RESPECTIVELY.  THERE ARE NXY
C VERTICES IN XY BUT THE LAST IS A DUPLICATE OF THE FIRST.
C ALL COORDINATES ARE FIXED POINT INTEGERS.
C
C THE RESULT IS RETURNED AS FOLLOWS.
C IN=0, P LIES OUTSIDE THE POLYGON.
C IN=1, P LIES ON THE POLYGON BOUNDARY.
C IN=2, P LIES IN THE POLYGON INTERIOR.
C
C WE USE THE METHOD OF J. D. JACOBSEN (IBM SYSTEMS JOURNAL,
C NUMBERS 3 AND 4, 1968, PAGES 333-334).  THE BASIC IDEA IS THAT A RAY
C IN ANY DIRECTION FROM P WILL INTERSECT THE BOUNDARY AN ODD NUMBER OF
C TIMES IF P LIES WITHIN THE BOUNDARY, AND AN EVEN NUMBER OF TIMES IF P
C LIES OUTSIDE THE BOUNDARY.  WE CONSIDER THE BOUNDARY IN THE COORDINATE
C SYSTEM HAVING P AS ORIGIN AND FOR CONVENIENCE THE RAY IS THE POSITIVE
C Y AXIS.
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
      REAL PP(2),X1,Y1,X2,Y2,XX
      IF(NXY.LT.4) GO TO 100
      DO 10 J=2,NXY
      IF(ISW.EQ.1) then
          call sspoints('X ',x,xj1,(j-1+iofx),1)
          X2=Xj1-PP(1)
      endif 
      IF(ISW.EQ.2) then
          call sspoints('P ',p,pj1,(j-1+iofp),1)
          X2=PJ1-PP(1)
      endif
       IF(X2.NE.0) GO TO 15
 10   continue   
      GO TO 100
  15  CONTINUE
      IF(ISW.EQ.1) then
           call sspoints('Y ',y,yj1,(j-1+iofy),1)
           Y2=YJ1-PP(2)
      endif
      IF(ISW.EQ.2) then
           call sspoints('Q ',q,qj1,(j-1+iofq),1)
           Y2=QJ1-PP(2)
      endif
      M=0
      DO 90 JJ=2,NXY
      X1=X2
      Y1=Y2
      IF(ISW.EQ.2) GO TO 1
      call sspoints('X ',x,xj,(j+iofx),1)
      X2=XJ-PP(1)
      call sspoints('Y ',y,yj,(j+iofy),1)
      Y2=YJ-PP(2)
      GO TO 2
 1    call sspoints('P ',p,pj,(j+iofp),1)
      X2=PJ-PP(1)
      call sspoints('Q ',q,qj,(j+iofq),1)
      Y2=QJ-PP(2)
 2    CONTINUE
      IF(X1) 21,22,23
   21 IF(X2) 80,50,30
   22 IF(X2) 40,60,40
   23 IF(X2) 30,50,80
   30 IF(Y1) 31,32,33
   31 IF(Y2) 80,80,35
   32 IF(Y2) 80,101,70
   33 IF(Y2) 35,70,70
   35 IF((X2*Y1-X1*Y2)*XSIGN(X2-X1)) 80,101,70
   40 IF(XSIGN(XX).NE.XSIGN(X2).AND.Y1.GT.0) GO TO 70
      GO TO 80
   50 IF(Y2.EQ.0) GO TO 101
      XX=X1
      GO TO 80
   60 IF(Y1*Y2) 101,101,80
   70 M=M+1
   80 IF(J.EQ.NXY) J=1
   90 J=J+1
      IF(MOD(M,2).EQ.1) GO TO 102
  100 IN=0
      GO TO 200
  101 IN=1
      GO TO 200
  102 IN=2
  200 RETURN
      END
