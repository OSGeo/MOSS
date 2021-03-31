      SUBROUTINE whwnd(vx1,vy1,vx2,vy2)
C
C *****************************************************************************
C
C             S U B R O U T I N E     W H W N D
C FUNCTION:
C             THIS ROUTINE RETURNS THE CURRENT DIMENSIONS OF THE WINDOW
C             TO THE CALLING PROGRAM IN VX1,VY1,VX2,VY2
C PARAMETERS:
C             VX1       OUT - THE X MINIMUN VALUE.
C             VY1       OUT - THE Y MINIMUN VALUE.
C             VX2       OUT - THE X MAXIMUN VALUE.
C             VY2       OUT - THE Y MAXIMUN VALUE.
C ROUTINES
C CALLED:     NONE.
C
C *****************************************************************************
C
      COMMON /vwnd  / xorgin , yorgin , xalen , yalen , xstart , 
     &                ystart , xlen , ylen , ibw
      IF ( ibw.EQ.1 ) THEN
         vx1 = xstart
         vy1 = ystart
         vx2 = vx1 + xlen
         vy2 = vy1 + ylen
      ELSE
         vx1 = xorgin
         vy1 = yorgin
         vx2 = vx1 + xalen
         vy2 = vy1 + yalen
      ENDIF
C
      RETURN
      END
