 
      REAL FUNCTION xsign(x)
C
C  IF X.LT.0, XSIGN=-1
C  IF Q.EQ.0, XSIGN=0
C  IF Q.GT.0, XSIGN=1
C
      IF ( x.LT.0 ) THEN
         xsign = -1.0
         RETURN
      ELSEIF ( x.EQ.0 ) THEN
         xsign = 0.0
         RETURN
      ELSE
         xsign = 1.0
C
         RETURN
      ENDIF
      END
