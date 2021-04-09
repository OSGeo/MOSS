
C*******************************************************************
C
C              S U B R O U T I N E    P R F A X I S
C
C  FUNCTION:  PLOT THE AXIS AND TEXT FOR A PROFILE
C
C********************************************************************
C
      SUBROUTINE PRFAXIS
C
C
      INTEGER*2 IMESS(50),MAPX(4)
      COMMON /PVALSS/ XDATA(512),YDATA(512),ZDATA(512),NVALS,MXVALS
      COMMON /IO/ NPRNT, IOIN
      COMMON /PRFDATA/ ZMAX,ZMIN,MAXDIST,VERTX,ZRINC,ZRANGE,ZFAC,XRINC
      COMMON /FLS/ IUNIT, IFILE(5)
      common /comext/keycom,facx,facy
C
C
       zero = 0.0
       izero = 0
c for 4010 emulation       facy=180.0
c for 4010 emulation       facx=108.0
       facx =  66.0
       facy = 108.0
C
C  INITIALIZE GRAPHICS DEVICE
C
C      CALL PLOTS (9600,IUNIT,IFILE)
C
C       CALL INITT (1 ,IUNIT,IFILE)
C  DO X AXIS LABELING
C
      IMESS(1) = 2HDI
      IMESS(2) = 2HST
      IMESS(3) = 2HAN
      IMESS(4) = 2HCE
      IMESS(5) = 2H (
      IMESS(6) = 2HMI
      IMESS(7) = 2HLE
      IMESS(8) = 2HS)
      IMESS(9) = 0
C
C      CALL AXIS(1.0,1.0,'DISTANCE (MILES)',16,8.,0.0,0.0,XRINC)
      xstart = .75
      ystart = .4
c      xstart = 100.0
c      ystart = 100.0
c
c       xlen = 900.0
       xlen = 8.0
       nchar = 16
       CALL AXIS(xstart,ystart,IMESS(1),nchar,xlen,zero,zero,XRINC)
C
C  DO Y AXIS LABELING
C
c      CALL TKPLOT (1.0,1.0,3)
c      CALL TKPLOT (1.0,1.5,2)
C
c       call flsh
C       CALL INITT (1 ,IUNIT,IFILE)
      xstart = .75
      ystart = .4
      CALL TKPLOT (xstart,ystart,3)
C
       ystart = ystart + 0.4
      CALL TKPLOT (xstart,ystart,2)
C
      IMESS(1) = 0
c      IMESS(1) = 1HZ
c needs to be 005a for Qhsymbx
      imess(1) = 32
      IMESS(2) = 0
C
C      CALL AXIS(1.0,1.5,'Z',1,ZRANGE*ZFAC,90.0,ZMIN,ZRINC)
C
       zpu = zrange*zfac
       nchar = 1
       angle = 90.0
       CALL AXIS(xstart,ystart,IMESS(1),nchar,ZPU,angle,ZMIN,ZRINC)
C
C      CALL PLOT(9.0,ZRANGE*ZFAC+1.5,3)
      ZPU = (ZRANGE*ZFAC)+1.0 
      XSTART = 8.75
C
CSKG        CALL TKPLOT(9.0,ZPU,3)
CSKG        CALL TKPLOT(9.0,1.0,2)
C
      ONE = .40
      CALL TKPLOT(XSTART,ZPU,3)
      CALL TKPLOT(XSTART,ONE,2)
c      CALL flsh
C
C  WRITE THE MIN,MAX VALUES IN LOWER-LEFT CORNER
C
C
      IMESS(1) = 2HMI
      IMESS(2) = 2HNI
      IMESS(3) = 2HMU
      IMESS(4) = 2HM
      IMESS(5) = 2HZ=
      IMESS(6) = 8224
      IMESS(7) = 0
C
       half = .15
       var12 = 0.10
       ione = 1
       nchar = 12
       var01 = 0.04
       CALL plotsym(izero,ione,1.0,half,var12,var12,IMESS(1),
     .             zero,zero,zero,izero,nchar,IER)
C
      IMESS(1) = 2HMA
      IMESS(2) = 2HXI
      IMESS(3) = 2HMU
      IMESS(4) = 2HM
      IMESS(5) = 2HZ=
      IMESS(6) = 8224
      IMESS(7) = 0
C
      var01 = 0.15
      CALL plotsym(izero,ione,3.25,half,var12,var12,IMESS(1),
     +            zero,zero,zero,izero,nchar,IER)
      barf = 1.96
      CALL PNUMBER(2.3,var01,var12,ZMIN,zero,ione)
      CALL PNUMBER(4.4,var01,var12,ZMAX,zero,ione)
C
      IMESS(1) = 2HVE
      IMESS(2) = 2HRT
      IMESS(3) = 2HIC
      IMESS(4) = 2HAL
      IMESS(5) = 2H E
      IMESS(6) = 2HXA
      IMESS(7) = 2HGG
      IMESS(8) = 2HER
      IMESS(9) = 2HAT
      IMESS(10) = 2HIO
      IMESS(11) = 2HN=
      IMESS(12) =  0
C
      nchar = 22
      barf = 5.5
      CALL plotsym(izero,1,barf,half,var12,var12,IMESS(1),
     .       zero,zero,zero,izero,nchar,IER)
      barf = 8.0
       var12 = 0.10
      CALL PNUMBER(barf,half,var12,VERTX,zero,ione)
 9011  continue
c      CALL flsh
C
      RETURN
      END
