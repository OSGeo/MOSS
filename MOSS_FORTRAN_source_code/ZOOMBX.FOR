 
 
      SUBROUTINE zoombx(jchan)
      COMMON /io    / nprnt , ioin
$Include:'vwnd.inc'
      CHARACTER*1 igoo
C
C *********************************************************
C
C             S U B R O U T I N E    Z O O M B X
C
C  FUNCTION: THIS ROUTINE ALLOWS THE USER TO DEFINE A ZOOM
C            BOX FOR MAP MAGNIFICATION.
C ********************************************************
C
      WRITE (nprnt,99001)
99001 FORMAT (' Use CURSOR (and space bar) to point to upper left and ',
     &        /,
     &    ' lower right corners of a new display window - use CR after '
     &    ,/,' viewing results.',//,' Give a CR to proceed'\)
      CALL readin(icard,1,icardp,istop)
      REWIND (jchan)
      WRITE (*,*) char(27) , char(12)
      CALL drfast(jchan,ier)
C
C  GET CORNER COORDINATES OF VIEWING WINDOW
C
      CALL vcursr(it,x1,y1)
      CALL xxx(x1,y1)
      CALL vcursr(it,x2,y2)
      CALL xxx(x2,y2)
C
C  DETERMINE PROPER INFORMATION FOR VIRTUAL WINDOW
C  SPECIFICATION
C
      xstart = amin1(x1,x2)
      ystart = amin1(y1,y2)
      xlen = abs(x1-x2)
      ylen = abs(y1-y2)
      ibw = 1
C
C  DRAW A RECTANGLE ABOUT THE AREA THE USER IS INTERESTED IN
C
      CALL abxdrw(xstart,xlen,ystart,ylen)
      CALL avwind(xstart,xlen,ystart,ylen)
      CALL flsh
C
      READ (*,'(a1)') igoo
      WRITE (*,*) char(27) , char(12)
      RETURN
      END
