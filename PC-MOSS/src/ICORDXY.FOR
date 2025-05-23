      SUBROUTINE icordxy(ibuff,nvert,nrec,xmin,ymin,ichan)
$Include:'io.inc'
$Include:'xypnts.500'
      DIMENSION ibuff(128) , ir3(2)
      EQUIVALENCE (ir3(1),scale)
C
C *****************************************************************
C
C             S U B R O U T I N E    C O R D X Y
C
C  FUNCTION - READ A COORDINATE STRING TO DISK
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
C ******************************************************************
C
      kpar = 64
      itype = ibuff(6)
      IF ( itype.GE.11 .AND. itype.LE.13 ) kpar = 42
      index = nvert
      nvert = ibuff(25)
      ilnds = ibuff(26)
C
      CALL scaget(ibuff(1),scale)
      ipoint = 0
C
C  ENTER READ LOOP
C
      index = nrec + 1
      IF ( ilnds.GT.99 ) index = index + ((ilnds-99)/128) + 1
 100  CONTINUE
      CALL rdblk(ichan,index,ibuff,1,ier)
      IF ( ier.NE.0 ) WRITE (nprnt,99001) index
      index = index + 1
      ic = 0
      DO i = 1 , kpar
         ic = ic + 1
         ix = ibuff(ic)
         ic = ic + 1
         iy = ibuff(ic)
         xt = (float(ix)*scale) + xmin
         yt = (float(iy)*scale) + ymin
         ipoint = ipoint + 1
         CALL sspoints('X ',x,xt,ipoint,2)
         CALL sspoints('Y ',y,yt,ipoint,2)
c        x(ipoint)=xt
c        y(ipoint)=yt
C        IF(KPAR.EQ.64) GO TO 43
C        IC=IC+1
C        ZT=IBUFF(IC)
         IF ( ipoint.GE.nvert ) THEN
C
C  AND RETURN
C
            RETURN
         ENDIF
      ENDDO
      GOTO 100
C
C  ERROR RETURN
C
99001 FORMAT (' *CORDXY*  ERROR FROM RDBLK OF RECORD',I5)
      END
