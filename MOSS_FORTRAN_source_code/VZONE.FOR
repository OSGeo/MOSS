        SUBROUTINE VZONE(DIST,NPTS,kkk,NVERT)
$Include:'xypnts.inc'
$Include:'rspnts.inc'
$Include:'pqpnts.inc'
        COMMON /IO/ NPRNT,IOIN
C
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
C                 S U B R O U T I N E    V Z O N E
C
C  FUNCTION:  THIS ROUTINE CALCULATES A LINE PARALELL BUT OFFSET
C             FROM AN INPUT LINE.
C
C  kkk = 1 do r and s points
C      = 2 do p and q points
C
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
C
C  IF DIST = 0, SET ERROR FLAG AND RETURN
C
      IER=1
      IF(DIST.EQ.0.0) RETURN
      ier=0
C
C  SET NEW LINE COORDINATE COUNTER
C
      NVERT=0
C
C  ENTER MAIN COMPUTATION LOOP
C
      LOW=1
      IF(LOW.EQ.NPTS) RETURN
C
C  DO FIRST POINT IN LINE
C
       call spoints('X ',x,xlow1,low+1,1)
       call spoints('X ',x,xlow, low,  1)
       call spoints('Y ',y,ylow1,low+1,1)
       call spoints('Y ',y,ylow, low,  1)
       ANG=ATAN2(YLOW1-YLOW,XLOW1-XLOW)
       XP=XLOW + DIST*SIN(-ANG)
       YP=YLOW + DIST*COS(-ANG)
C
C  STORE COORDINATE
C
        NVERT=NVERT+1
        if(kkk.eq.1)then
c r and s
          call spoints('R ',r,xp,nvert,2)
          call spoints('S ',s,yp,nvert,2)
c        XT(NVERT)=XP
c        YT(NVERT)=YP
        else
c p & q points
          call spoints('P ',p,xp,nvert,2)
          call spoints('Q ',q,yp,nvert,2)
        endif
C
C  CHECK FOR TWO POINT LINE
C
        IF(NPTS.EQ.2) GO TO 146
C
C  PROCESS REST OF COORDINATES IN INPUT LINE
C
        DO 145 J=2,NPTS-1
C
C         GET POINTS FOR FIRST AND SECOND SEGMENTS
C
          call spoints('X ',x,x1,j-1,1)
          call spoints('Y ',y,y1,j-1,1)
          call spoints('X ',x,x2,j,1)
          call spoints('Y ',y,y2,j,1)
          call spoints('X ',x,x3,j+1,1)
          call spoints('X ',y,y3,j+1,1)
c        X1=X(J-1)
c        Y1=Y(J-1)
c        X2=X(J)
c        Y2=Y(J)
c        X3=X(J+1)
c        Y3=Y(J+1)
C
C  CALCULATE ANGLES FOR FIRST AND SECOND SEGMENTS
C
        if(y2-y1.ne.0.0.or.x2-x1.ne.0.0)then
            ANG1=ATAN2(Y2-Y1,X2-X1)
        else
            ang1=0.0
        endif
        if(y3-y2.ne.0.0.or.x3-x2.ne.0.0)then
            ANG2=ATAN2(Y3-Y2,X3-X2)
        else
            ang2=0.0
        endif
C  CALCULATE UN-ADJUSTED ENDPOINT
C
        X11=X1 + DIST*SIN(-ANG1)
        Y11=Y1 + DIST*COS(-ANG1)
        X33=X3 + DIST*SIN(-ANG2)
        Y33=Y3 + DIST*COS(-ANG2)
C
C  CALCLUATE DELTAX AND DELTY FOR EACH SEGMENT
C
        DX1=X2-X1
        DY1=Y2-Y1
        DX2=X3-X2
        DY2=Y3-Y2
C
C  LINE SEGMENT IS VERTICAL
C
        IF(DX1.EQ.0.) GO TO 115       
        S1=DY1/DX1
C
C  SECOND LINE SEGMENT IS VERTICAL
C
        IF(DX2.EQ.0.) GO TO 116       
        S2=DY2/DX2
C
C  CHECK FOR PARALLEL LINES
C
        IF(ABS(S1-S2).LT..1) GO TO 114
C
C  NORMAL SEGMENT.  CALCULATE NEW X VALUE
C
        XP=(S1*X11 - S2*X33 - Y11 + Y33)/(S1-S2)
        GO TO 117
 114    CONTINUE
        IS1=SIGN(1.0,ANG1)
        IS2=SIGN(1.0,ANG2)
        IF(ABS(S1).LT.0.05.AND.IS1.NE.IS2) ANG1=-ANG1
        IF(ABS(S2).LT.0.05.AND.IS1.NE.IS2) ANG2=-ANG2
C        TYPE ' IS1 AND IS2 = ',IS1,IS2
C        TYPE ' SLOPES ARE S1,S2 = ',S1,S2
C        TYPE ' ANGLES ARE ',ANG1,ANG2
        XP=X2 + DIST*SIN((-ANG1-ANG2)/2.)
        YP=Y2 + DIST*COS((-ANG1-ANG2)/2.)
C       TYPE ' DIST,OFFSET ' ,DIST,COS((-ANG1-ANG2)/2.)
        GO TO 118
 115    XP=X11
        YP=DY2/DX2*(XP-X33) + Y33
        GO TO 118
 116    XP=X33
 117    YP=S1*(XP-X11) + Y11
 118    CONTINUE
 140  CONTINUE
C
C  SAVE ANOTHER POINT
C
        NVERT=NVERT+1
        if(kkk.eq.1)then
c r and s
          call spoints('R ',r,xp,nvert,2)
          call spoints('S ',s,yp,nvert,2)
c        XT(NVERT)=XP
c        YT(NVERT)=YP
        else
c p & q points
          call spoints('P ',p,xp,nvert,2)
          call spoints('Q ',q,yp,nvert,2)
        endif
 145    CONTINUE
 146    CONTINUE
        call spoints('X ',x,xnpts,npts,1)
        call spoints('Y ',y,ynpts,npts,1)
        call spoints('X ',x,xnpts1,npts-1,1)
        call spoints('Y ',y,ynpts1,npts-1,1)
        ANG=ATAN2(YNPTS-YNPTS1,XNPTS-XNPTS1)
        XP=XNPTS + DIST*SIN(-ANG)
        YP=YNPTS + DIST*COS(-ANG)
C
C  LAST POINT
C
        NVERT=NVERT+1
        if(kkk.eq.1)then
c r and s
          call spoints('R ',r,xp,nvert,2)
          call spoints('S ',s,yp,nvert,2)
c        XT(NVERT)=XP
c        YT(NVERT)=YP
        else
c p & q points
          call spoints('P ',p,xp,nvert,2)
          call spoints('Q ',q,yp,nvert,2)
        endif
        ier=0
        RETURN
        END
