      SUBROUTINE zoompt(jchan)
      COMMON /io    / nprnt , ioin
$Include:'strng.inc'
$Include:'vwnd.inc'
C
C *************************************************************
C
C
C            S U B R O U T I N E    Z O O M P T
C
C FUNCTION:  ALLOW USER TO SPECIFY POINT AND MAGNIFICATION
C            VALUE FOR ZOOM ON MAP DATA.
C *************************************************************
C
      lines = 2
C
C  SEE IF USER HAS ALREADY ENTERED MAG. VALUE
C
      CALL fnum(icard,80,rmag,k)
      IF ( rmag.LE.0.0 ) THEN
C
C  NOPE.  ASK FOR MAG VALUE AND UPDATE CURSOR COUNT
C
         WRITE (nprnt,99001)
99001    FORMAT (' Please enter MAGNIFICATION factor')
C
         CALL readin(icard,lenicd,k,istop)
         CALL fnum(icard,80,rmag,k)
         IF ( rmag.LE.0.0 ) THEN
C
C  ERRORS
C
            WRITE (nprnt,99002)
99002       FORMAT (' Mag factor must be greater than 0')
            lines = lines + 1
C      CALL CPLAC(LINES)
C      CALL POSTF
C
            RETURN
         ENDIF
      ENDIF
C
C
C  GET ZOOM CENTER
C
      WRITE (nprnt,99003)
99003 FORMAT (' Please point to ZOOM center- use CR to proceed')
      CALL readin(icard,lenicd,icardp,istop)
C
      lines = lines + 2
      REWIND (jchan)
      WRITE (*,*) char(27) , char(12)
      CALL drfast(jchan,ier)
C
      CALL vcursr(it,x1,y1)
      CALL xxx(x1,y1)
C
C  RESET ZOOM WINDOW BASED ON CURRENT WINDOW VALUES
C
      IF ( ibw.EQ.1 ) THEN
C
         xt = xlen/rmag
         yt = ylen/rmag
      ELSE
C
         xt = xalen/rmag
         yt = yalen/rmag
      ENDIF
C
C
      ibw = 1
      xstart = x1 - (xt/2.0)
      ystart = y1 - (yt/2.0)
      xlen = xt
      ylen = yt
C
C
C   UPDATE Cursor POSITION
C
CC      CALL CPLAC(LINES)
CC     CALL POSTF
 
C      call flsh
C  AND UPDATE WINDOW INFORMATION IN GRAPHICS WORLD
C
      CALL avwind(xstart,xlen,ystart,ylen)
      RETURN
      END
