      SUBROUTINE AXIS(XPAGE,YPAGE,IBCD,NCHAR,
     .                AXLEN,ANGLE,FIRSTV,DELTAV)
C
C.....     XPAGE,YPAGE  COORDINATES OF STARTING POINT OF AXIS, IN INCHES
C.....     IBCD         AXIS TITLE.
C.....     NCHAR        NUMBER OF CHARACTERS IN TITLE. + FOR C.C-W SIDE.
C.....     AXLEN        FLOATING POINT AXIS LENGTH IN INCHES.
C.....     ANGLE        ANGLE OF AXIS FROM THE X-DIRECTION, IN DEGREES.
C.....     FIRSTV       SCALE VALUE AT THE FIRST TIC MARK.
C.....     DELTAV       CHANGE IN SCALE BETWEEN TIC MARKS ONE INCH APART
C
C.....   MODIFIED:  S.K. GOODMAN, AUTOMETRIC, APR 88 ( PC-MOSS )
C.....   MODIFIED:  S.K. GOODMAN, AUTOMETRIC, MAY 88 ( hsymb )
c.....              s.k. goodman, autometric, may 88 ( no stack vars )
c.....              s.k. goodman, autometric, may 88 ( scaling changes )
C
      INTEGER*2 IMESS(20),IBCD(40)
      common /ax/ a,kn,ex,adx,xval,cth,sth,dxb,dyb,xn,yn,xmath,ymath,
     +            ntic,nt,zp15,ndp,z,zero,xt,yt,i
      real*4      a,ex,adx,eval,sth,cth,dxb,dyb,xn,yn,xmath,ymath,zp15
      real*4      z,zero,xt,yt
      integer*2   kn,ntic,nt,ndp,i
C
C
      zero = 0.0
      KN=NCHAR
      A=1.0
      IF (KN) 1,2,2
  1   A=-A
      KN=-KN
  2   EX=0.0
      ADX= ABS(DELTAV)
      go to 7
      IF (ADX) 3,7,3
  3   IF (ADX- 99.0) 6,4,4
  4   ADX=ADX/10.0
      EX=EX+1.0
      GO TO 3
  5   ADX=ADX*10.0
      EX=EX-1.0
  6   IF (ADX-0.01) 5,7,7
  7   XVAL=FIRSTV*10.0**(-EX)
      ADX= DELTAV*10.0**(-EX)
      STH=ANGLE*0.0174533
      CTH=COS(STH)
      STH=SIN(STH)
      DXB=-0.1
      DYB=0.15*A-0.05
      XN=XPAGE+DXB*CTH-DYB*STH
      YN=YPAGE+DYB*CTH+DXB*STH
      NTIC=AXLEN+1.0
      NT=NTIC/2
      DO  20  I=1,NTIC
        zp15 = 0.10
        if(angle.gt.0.0)zp15=.08
        ndp = 2
c
        CALL PNUMBER(XN,YN,zp15,XVAL,ANGLE,ndp)
        XVAL=XVAL+ADX
        XN=XN+CTH
        YN=YN+STH
        IF (NT) 20,11,20
 11       Z = float( KN )
          IF (EX)  12,13,12
 12       Z=Z+7.0
 13       DXB=-.07*Z+AXLEN*0.5
          DYB=0.325*A-0.075
          XT=XPAGE+DXB*CTH-DYB*STH
          YT=YPAGE+DYB*CTH+DXB*STH
C**          CALL SYMBOL(XT,YT,0.14,IBCD(1),ANGLE,KN)
c
          zp15 = 0.12
          CALL plotsym(0,1,Xt,Yt,zp15,zp15,IBCD(1),ANGLE,zero,
     .         ANGLE,0,KN,IER)
          IF (EX)  14,20,14
 14         Z= float( KN )
            z = z + 2.0
            XT=XT+Z*CTH*0.14
            YT=YT+Z*STH*0.14
C**            CALL SYMBOL(XT,YT,0.14,3H*10,ANGLE,3)
            IMESS(1) = 2H*1
            IMESS(2) = 8224
            IMESS(2) = 1H0
            IMESS(3) =  0
c
            zp15 = 0.12
            CALL plotsym(0,1,XT,Yt,zp15,zp15,IMESS(1),ANGLE,zero,
     .           ANGLE,0,3,IER)
            XT=XT+(3.0*CTH-0.8*STH)*0.14
            YT=YT+(3.0*STH+0.8*CTH)*0.14
            zp15 = 0.05
            ndp = 0
c            ndp = -1
c            CALL PNUMBER(XT,YT,zp15,EX,ANGLE,-1)
            CALL PNUMBER(XT,YT,zp15,EX,ANGLE,ndp)
 20   NT=NT-1
      XMATH = XPAGE+AXLEN*CTH
      YMATH = YPAGE+AXLEN*STH
      CALL TKPLOT(XMATH,YMATH,3)
      DXB=-0.07*A*STH
      DYB=+0.07*A*CTH
      A= FLOAT(NTIC-1)
      XN=XPAGE+A*CTH
      YN=YPAGE+A*STH
      DO  30  I=1,NTIC
        CALL TKPLOT(XN,YN,2)
        XMATH = XN+DXB
        YMATH = YN+DYB
        CALL TKPLOT(XMATH,YMATH,2)
        CALL TKPLOT(XN,YN,2)
        XN=XN-CTH
        YN=YN-STH
 30   CONTINUE
      RETURN
      END
