      SUBROUTINE abxdrw(xstart,xlen,ystart,ylen)
C
C  DRAW A BOX IN THE SCREEN OF ORIGIN (XSTART,YSTART)
C  A LENGTH XLEN IN X AND LENGTH YLEN IN Y
C
C  THIS ROUTINE IS FOR PLOT10 COMPATIBLE TERMINALS
C
      CALL movea(xstart,ystart)
      CALL drawa(xstart+xlen,ystart)
      CALL drawa(xstart+xlen,ystart+ylen)
      CALL drawa(xstart,ystart+ylen)
      CALL drawa(xstart,ystart)
C
C      CALL POSTF
C
      RETURN
      END
