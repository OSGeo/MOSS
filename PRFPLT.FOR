 
C********************************************************************
C
C              S U B R O U T I N E    P R F P L T
C
C  FUNCTION:  CALCUATE VALUES NEEDED FOR PLOTTING MULTIPLE PROFILES
C
C*********************************************************************
C
      SUBROUTINE prfplt
C
C
      COMMON /pvalss/ xdata(512) , ydata(512) , zdata(512) , nvals , 
     &                mxvals
      COMMON /prfdata/ zmax , zmin , maxdist , vertx , zrinc , zrange , 
     &                 zfac , xrinc
      COMMON /io    / nprnt , ioin
      INTEGER iget(5)
C
      fdmax = float(maxdist)
c
c      XRINC=FLOAT(AINT(MAXDIST/8))*3.2808/5280.
c      XFAC = 8.0 / MAXDIST
c      XRINC = fdmax /8.0 * 3.2808 / 5280.0
c      XFAC = 8.0 / fdmax
      xrinc = fdmax/8.0*3.2808/5280.0
      xfac = 8.0/fdmax
C
      zr = 1.0
C      IF(IUNITS.EQ.0) ZR=12.0/39.37
      vertx = 1.0
      zrange = zmax - zmin
      zfac = xfac*vertx*zr
      zpu = zrange*zfac
      IF ( zpu.GE.2.95 .AND. zpu.LE.3.05 ) GOTO 300
      rnc = .05
      IF ( zpu.GT.3.0 ) rnc = (-.05)
 100  CONTINUE
      vertx = vertx + rnc
      IF ( vertx.GE..10 ) THEN
         zfac = xfac*vertx*zr
         zpu = zrange*zfac
         IF ( rnc.LT.0.0 .AND. zpu.GT.3.0 ) GOTO 100
         IF ( rnc.GT.0.0 .AND. zpu.LT.3.0 ) GOTO 100
      ENDIF
 200  CONTINUE
      WRITE (*,99001) vertx
99001 FORMAT (' A Vertical Exaggeration of: ',F4.1,' will allow',/,
     &      ' the Profile to fit the screen - use it (CR) or input new '
     &      ,'value:'\)
      CALL readin(iget,5,is,ic)
      IF ( iget(1).NE.0 ) THEN
         k = 1
         CALL fnum(iget,5,vertxx,k)
         IF ( vertxx.LT.0.0 ) GOTO 200
         vertx = vertxx
         zfac = xfac*vertx*zr
      ENDIF
 300  CONTINUE
      kval = (zrange*zfac) + .5
      IF ( kval.LT.1 ) kval = 1
      IF ( kval.GT.8 ) kval = 8
      zrinc = (abs(zrange))/float(kval)
      zrinc = abs(zrinc)
C
      RETURN
      END
