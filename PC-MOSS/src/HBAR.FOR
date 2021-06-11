C*************************************************************************
C
C      DRAWS SCALE BAR OF SPECIFIED HEIGHT & WIDTH WITH VARIOUS HATCHING
C      PROGRAMMED BY USFWS (WELUT)  04-JUL-79
C      SUBROUTINES CALLED:  PLOT (CALCOMP)
C
C      PARAMETERS:
C
C      XPAGE       - THE COORDINATES OF THE LOWER LEFT CORNER IN INCHES
C      YPAGE
C      ANGLE      - ANGLE OF THE BASE OF THE BAR.  ROTATED ABOUT XPAGE,YPAGE
C      HEIGHT      - HEIGHT OF MAIN BAR IN INCHES
C      WIDTH      - WIDTH OF BAR IN INCHES
C      SH      - HEIGHT OF INTERMEDIATE BAR IN INCHES
C      IHAT      - HATCHING CODE FOR INTERMEDIATE BAR
C              1 - DRAW BAR ONLY
C              2 - HATCH LEFT TO RIGHT
C              3 - HATCH FROM RIGHT TO LEFT
C              4 - HATCH BOTH WAYS
C      NPI      - NUMBER OF LINES OF HATCHING PER INCH
C
C**************************************************************************
C
C
      SUBROUTINE hbar(x,y,th,h,w,sh,ihat,npi)
      theta = th*0.017453292
      xs = sin(theta)
      xc = cos(theta)
      ak = w*xc
      bk = w*xs
      CALL thick(x,y,3)
      x1 = x - h*xs
      y1 = y + h*xc
      CALL thick(x1,y1,2)
      x1 = x1 + ak
      y1 = y1 + bk
      CALL thick(x1,y1,2)
      x1 = x + ak
      y1 = y + bk
      CALL thick(x1,y1,2)
      CALL thick(x,y,2)
C      SET FLAGS ACCORDING TO IHAT
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
         x2 = x - sh*xs
         y2 = y + sh*xc
         x3 = x2 + ak
         y3 = y2 + bk
         CALL thick(x3,y3,k1)
         CALL thick(x2,y2,2)
         CALL thick(x1,y1,k2)
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
                  DO i = 1 , ih
                     h1 = h1 + dh
                     h2 = sh - h1
                     w2 = w2 + dw
                     w1 = w - w2
                     xp1 = x + w1*xc
                     yp1 = y + w1*xs
                     CALL thick(xp1,yp1,3)
                     xk1 = x - h2*xs
                     yk1 = y + h2*xc
                     CALL thick(xk1,yk1,k2)
C     call thick (X - H2 * XS, Y + H2 * XC, K2)
                     xk1 = x2 + w2*xc
                     yk1 = y2 + w2*xs
                     CALL thick(xk1,yk1,k1)
C      call thick (X2 + W2 * XC, Y2 + W2 * XS, K1)
C      call thick (X1 - H1 * XS, Y1 + H1 * XC, K2)
                     xk1 = x1 - h1*xs
                     yk1 = y1 + h1*xc
                     CALL thick(xk1,yk1,k2)
                     CALL thick(xp1,yp1,k1)
                  ENDDO
               ENDIF
            ENDIF
         ENDIF
      ENDIF
 100  CONTINUE
      RETURN
      END
