      SUBROUTINE adcord(iarr,nvert,nrec,xxmin,yymin,ichan,scale,izz)
      COMMON /io    / nprnt , ioin
$Include:'xypnts.inc'
C$Include:'yryr.inc'
      COMMON /cstfc / iz , ipoint , sc , xmin , ymin
      DIMENSION ibuff(128)
      DIMENSION iarr(128) , ir3(2)
      EQUIVALENCE (sc,ir3(1))
C
C *****************************************************************
C
C             S U B R O U T I N E    A D C O R D
C
C  FUNCTION - WRITE A COORDINATE STRING TO DISK
C
C  INPUT PARAMETERS:
C       IBUFF  - 128 WORD WORK SPACE
C       NVERT - NUMBER OF COORDINATE PAIRS IN ITEM
C       NREC  - RECORD POINTTER
C                   IN = CURRENT POSITION
C                  OUT = POSITION AFTER COORDINATE READ
C       XMIN  - MIN X OF ITEM
C       YMIN  - YMIN OF ITEM
C      ICHAN  - DATA CHANNEL NUMBER ON WHICH TO READ
C
C ******************************************************************
C
C
C  SET 2D/3D COUNTER
C
      xmin = xxmin
      ymin = yymin
      kpar = 64
      IF ( iz.EQ.1 ) kpar = 42
      IF ( iz.EQ.2 ) kpar = 32
      iz = izz
      sc = scale
C
      ilnds = iarr(26)
C
      iarr(127) = ir3(1)
      iarr(128) = ir3(2)
C
      inc = 1
      IF ( ilnds.NE.0 ) THEN
         inc = ((ilnds-99)/128) + 1
         IF ( ilnds.GT.99 ) inc = inc + 1
      ENDIF
      CALL wrblk(ichan,nrec-inc,iarr,1,ier)
      IF ( ier.NE.0 ) WRITE (nprnt,99001) nrec - inc
C
C
C  ZERO WORK ARRAY
C
      CALL clear(ibuff,128)
C
C  SET COUNTERS FOR TRANSFER LOOP
C
      kount = 0
      ic = 0
C
C
C  ENTER TRANSFER LOOP
C
      DO ipoint = 1 , nvert
C
         kount = kount + 1
C
         CALL spoints('X ',x,xtt,ipoint,1)
         CALL spoints('Y ',y,ytt,ipoint,1)
         xt = ((xtt-xmin)/sc)
         yt = ((ytt-ymin)/sc)
c          IF(IZ.EQ.1) then
c           call spoints('YR',yr,zt,ipoint,1)
C            ZT=YR(IPOINT)
c          endif
c
         xt = anint(xt)
         yt = anint(yt)
C
         ic = ic + 1
         ibuff(ic) = xt
         ic = ic + 1
         ibuff(ic) = yt
C
c          IF(IZ.NE.1) GO TO 10
C
c            IC=IC+1
c            IBUFF(IC)=ZT
C
         IF ( kount.GE.kpar ) THEN
C
            ic = 0
            kount = 0
            CALL wrblk(ichan,nrec,ibuff,1,ier)
            CALL clear(ibuff,128)
            nrec = nrec + 1
         ENDIF
C
      ENDDO
      IF ( kount.NE.0 ) THEN
C
         CALL wrblk(ichan,nrec,ibuff,1,ier)
         nrec = nrec + 1
      ENDIF
C
C  AND RETURN
C
      RETURN
C
C
C
C  ERROR RETURN
99001 FORMAT (' *ADCORD*  ERROR FROM WRBLK OF RECORD',I5)
      END
