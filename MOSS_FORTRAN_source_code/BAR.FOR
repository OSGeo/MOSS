C****
C
C      DRAW SCALE BAR OF SPECIFIED HEIGHT & WIDTH WITH VARIOUS HATCHING
C
C      PARAMETERS:
C
C      XPAG     - THE COORDINATES OF THE LOWER LEFT CORNER IN INCHES
C      YPAG
C      ANGLE      - ANGLE OF THE BASE OF THE BAR.  ROTATED ABOUT
C
C      HEIGHT      - HEIGHT OF MAIN BAR IN INCHES
C      WIDTH      - WIDTH OF BAR IN INCHES
C      SH      - HEIGHT OF INTERMEDIATE BAR IN INCHES
C
C      IHAT      - HATCHING CODE FOR INTERMEDIATE BAR
C              1 - DRAW BAR ONLY
C              2 - HATCH LEFT TO RIGHT
C              3 - HATCH FROM RIGHT TO LEFT
C              4 - HATCH BOTH WAYS
C      NPI      - NUMBER OF LINES OF HATCHING PER INCH
C
      SUBROUTINE bar(xpag,ypag,th,h,w,sh,ihat,npi)
      theta = th*0.017453292
      xs = sin(theta)
      xc = cos(theta)
      ak = w*xc
      bk = w*xs
      CALL hpplot(xpag,ypag,3)
      x1 = xpag - h*xs
      y1 = ypag + h*xc
      CALL hpplot(x1,y1,2)
      x1 = x1 + ak
      y1 = y1 + bk
      CALL hpplot(x1,y1,2)
      x1 = xpag + ak
      y1 = ypag + bk
      CALL hpplot(x1,y1,2)
      CALL hpplot(xpag,ypag,2)
      IF ( sh.GT.0 ) THEN
         IF ( ihat.LT.1 ) GOTO 100
         IF ( ihat.EQ.1 ) THEN
            k1 = 3
            k2 = 3
         ELSEIF ( ihat.LT.3 ) THEN
            k1 = 2
            k2 = 3
         ELSEIF ( ihat.EQ.3 ) THEN
            k1 = 3
            k2 = 2
         ELSE
            k1 = 2
            k2 = 2
         ENDIF
C      HATCH ONE
         x2 = xpag - sh*xs
         y2 = ypag + sh*xc
         x3 = x2 + ak
         y3 = y2 + bk
         CALL hpplot(x3,y3,k1)
         CALL hpplot(x2,y2,2)
         CALL hpplot(x1,y1,k2)
         IF ( ihat.GT.1 ) THEN
            fnpi = npi
            l = w*fnpi
            m = sh*fnpi
            IF ( m.GT.0 ) THEN
               IF ( l.GT.0 ) THEN
                  IF ( l.LE.m ) THEN
                     f = l
                  ELSE
                     f = m
                  ENDIF
                  dh = sh/f
                  dw = w/f
                  w2 = 0.0
                  h1 = 0.0
                  ih = f - 1.0
C
                  DO i = 1 , ih
                     h1 = h1 + dh
                     h2 = sh - h1
                     w2 = w2 + dw
                     w1 = w - w2
                     xp1 = xpag + w1*xc
                     yp1 = ypag + w1*xs
                     CALL hpplot(xp1,yp1,3)
                     CALL hpplot(xpag-h2*xs,ypag+h2*xc,k2)
                     CALL hpplot(x2+w2*xc,y2+w2*xs,k1)
                     CALL hpplot(x1-h1*xs,y1+h1*xc,k2)
                     CALL hpplot(xp1,yp1,k1)
                  ENDDO
               ENDIF
            ENDIF
         ENDIF
      ENDIF
 100  CONTINUE
      RETURN
      END
