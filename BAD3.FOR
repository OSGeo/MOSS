C***************************************************************************
C
C  Name:    B A D 3
C
C  Purpose: DECIDES IF NEW BOX OVERLAPS ANY PREVIOUS ONES
C
C  Commons/Variables    In/Out        Attributes   Function/Size
C
C  -- Dummy Arguments --
C     BAD3              Out           Logical Entry - Result of function
C     TEMPBX            In            Real Array(8) - Label's box coordinates
C     RBM               In            Real Array(4) - MBR of label's box coords
C     IER               Out           Integer - Returned error code
C
C  -- IO --
C     NPRNT                           Integer
C     IOIN                            Integer
C  -- RBP --
C     IRBGP                           Integer
C     ICHP                            Integer
C
C  -- Static Variables --
C     IPDX                            Integer
C
C  -- Stack Variables --
C     TWO                             Logical
C     I                               Integer
C     IFLAG                           Integer
C     MFF                             Integer
C     ND                              Integer
C     ICOFF                           Integer
C     IPOFF                           Integer
C     XI                              Real Array             4
C     YI                              Real Array             4
C     XJ                              Real Array             4
C     YJ                              Real Array             4
C     XMIN                            Real
C     XMAX                            Real
C     YMIN                            Real
C     YMAX                            Real
C     XPMIN                           Real
C     YPMIN                           Real
C     XPMAX                           Real
C     YPMAX                           Real
C     DUM1                            Real
C     DUM2                            Real
C
C
      LOGICAL FUNCTION bad3(tempbx,rbm,ier)
C
$INCLUDE: 'PQPNTS.INC'
      COMMON /io    / nprnt , ioin
C
      DIMENSION tempbx(8) , rbm(4) , xi(2) , yi(2) , xj(2) , yj(2)
      LOGICAL two
      DATA ipdx/0/
C
      bad3 = .FALSE.
      ier = 0
      xmin = rbm(1)
      xmax = rbm(2)
      ymin = rbm(3)
      ymax = rbm(4)
C.....FIRST BOX, SIMPLY STASH AND RETURN, HENCE FORTH COMPARE AGAINST OTHERS
      DO i = 1 , 8
         ipdx = ipdx + 1
         CALL spoints('P ',p,tempbx(i),ipdx,2)
      ENDDO
c
C   50   P(IPDX) = TEMPBX(I)
C
      IF ( (ipdx-8).LE.0 ) GOTO 500
C.......SETUP PREVIOUS LABELS MBR
      nd = ipdx - 8
      mff = 0
 100  CONTINUE
      xpmin = 9999999.
      ypmin = 9999999.
      xpmax = -9999999.
      ypmax = -9999999.
      two = .FALSE.
      DO i = 1 , 8
         CALL spoints('P ',p,pimff,i+mff,1)
         IF ( .NOT.(two) ) THEN
            xpmin = amin1(xpmin,pimff)
            xpmax = amax1(xpmax,pimff)
         ENDIF
         IF ( two ) THEN
            ypmin = amin1(ypmin,pimff)
            ypmax = amax1(ypmax,pimff)
         ENDIF
         two = .NOT.two
      ENDDO
C.......LABELS MBR OVERLAP?
      IF ( invwnd(xpmin,xpmax,ypmin,ypmax,xmin,xmax,ymin,ymax).EQ.0 )
     &      GOTO 400
C.........LABELS MBRS OVERLAP, THOROUGH CHECK FOLLOWS
      icoff = -1
C.........CURRENT LABEL SEGMENT SETUP SECTION
 200  CONTINUE
      icoff = icoff + 2
      xi(1) = tempbx(icoff)
      yi(1) = tempbx(icoff+1)
      IF ( icoff+2.LE.7 ) xi(2) = tempbx(icoff+2)
      IF ( icoff+2.LE.7 ) yi(2) = tempbx(icoff+3)
      IF ( icoff+2.GT.7 ) xi(2) = tempbx(1)
      IF ( icoff+2.GT.7 ) yi(2) = tempbx(2)
      ipoff = mff - 1
C.........THIS PREVIOUS LABEL SEGMENT SETUP
 300  CONTINUE
      ipoff = ipoff + 2
      CALL spoints('P ',p,xj(1),ipoff,1)
      CALL spoints('P ',p,yj(1),ipoff+1,1)
      CALL spoints('P ',p,xj(2),ipoff+2,1)
      CALL spoints('P ',p,yj(2),ipoff+3,1)
c          XJ(1) = P( IPOFF )
c          YJ(1) = P(IPOFF+1)
c          XJ(2) = P(IPOFF+2)
c          YJ(2) = P(IPOFF+3)
C
      IF ( ipoff+2.GE.mff+8 ) THEN
         CALL spoints('P ',p,xj(2),mff+1,1)
         CALL spoints('P ',p,yj(2),mff+2,1)
c              XJ(2) = P(MFF+1)
c              YJ(2) = P(MFF+2)
      ENDIF
C.........SEGMENTS INTERSECT?
      CALL sinter(xi,yi,xj,yj,dum1,dum2,iflag,0.)
C.........IFLAG = 1 IS INTERSECTION
      IF ( iflag.NE.0 ) THEN
C
C.....PROBLEM EXIT
         bad3 = .TRUE.
         ipdx = ipdx - 8
         GOTO 500
      ELSE
C...........DONE ALL SEGMENTS THIS PREVIOUS LABEL?
         IF ( ipoff+3.LT.mff+8 ) GOTO 300
C.............DONE ALL SEGMENTS OF CURRENT LABEL
         IF ( icoff+3.LT.8 ) GOTO 200
      ENDIF
C
C.......NO PROBLEM THIS PREVIOUS, GET ANOTHER
 400  CONTINUE
      mff = mff + 8
C.......DONE ALL PREVIOUS LABELS?
      IF ( mff.LT.nd ) GOTO 100
C
 500  CONTINUE
      RETURN
      END
