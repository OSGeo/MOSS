      PROGRAM reset
C
      COMMON /vwnd  / xorgin , yorgin , xalen , yalen , xstart , 
     &                ystart , xlen , ylen , ibw
C
      CALL initl(ier)
      IF ( ier.NE.0 ) STOP
C
C
C  RESET DISPLAY AREA TO ORIGINAL WINDOW
C
      ibw = 0
      CALL setwnd
      CALL outcm
C
      END
