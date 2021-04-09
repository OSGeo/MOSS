      FUNCTION term(x,ex,y,ey,xmax,ymax)
C***  THIS FUNCTION WAS WRITTEN BY SKRIVAN AND KARLINGER
C***  CALCULATE MATRIX COEFFICIENTS BASED ON DRIFT TERMS.
C***  SCALE X BY 1./XMAX AND Y BY 1./YMAX TO ALLOW MORE SIMILAR
C***  COEFFICIENTS IN THE MATRIX.
C***  CHECK FOR 0 EXPONENT AND SET THAT PORTION OF TERM TO 1.
      INTEGER*2 ex , ey
      scalex = 1.
      scaley = 1.
      IF ( xmax.NE.0. ) scalex = 1./xmax
      IF ( ymax.NE.0. ) scaley = 1./ymax
      IF ( ex.NE.0 ) THEN
         tx = (x*scalex)**ex
      ELSE
         tx = 1.
      ENDIF
      IF ( ey.NE.0 ) THEN
         ty = (y*scaley)**ey
      ELSE
         ty = 1.
      ENDIF
      term = tx*ty
      RETURN
      END
