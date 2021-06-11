      SUBROUTINE sinter(xi,yi,xl,yl,xint,yint,iflag,tol)
      DIMENSION xi(2) , yi(2) , xl(2) , yl(2)
      iflag = 0
      xint = 0.
      yint = 0.
C  CHECK TO SEE WHETHER EXACTLY 1 LINE IS VERTICAL
      xidiff = xi(2) - xi(1)
      yidiff = yi(2) - yi(1)
      xldiff = xl(2) - xl(1)
      yldiff = yl(2) - yl(1)
      IF ( xldiff.EQ.0. .AND. xidiff.NE.0. ) THEN
         xx = xl(1)
         slope = yidiff/xidiff
         b = yi(1) - slope*xi(1)
      ELSEIF ( xidiff.NE.0. .OR. xldiff.EQ.0 ) THEN
C  CHECK TO SEE WHETHER BOTH LINES ARE VERTICAL
         IF ( xidiff.EQ.0. .AND. xldiff.EQ.0. ) RETURN
C  CHECK TO SEE WHETHER LINES ARE PARALLEL
         IF ( abs(yidiff/xidiff-yldiff/xldiff).LE..0001 ) GOTO 200
C  LINES ARE NOT PARALLEL AND NEITHER IS VERTICAL
         slopei = yidiff/xidiff
         slopel = yldiff/xldiff
         ycepi = yi(1) - slopei*xi(1)
         ycepl = yl(1) - slopel*xl(1)
         a = slopei - slopel
         xint = (ycepl-ycepi)/a
         IF ( abs(slopei).GT.abs(slopel) ) THEN
            yint = slopel*xint + ycepl
         ELSE
            yint = slopei*xint + ycepi
         ENDIF
         GOTO 100
      ELSE
         xx = xi(1)
         slope = yldiff/xldiff
         b = yl(1) - slope*xl(1)
      ENDIF
      xint = xx
      yint = slope*xx + b
C  CHECK TO SEE WHETHER XINT AND YINT LIE ON THE GIVEN
C  LINE SEGMENTS
 100  CONTINUE
      xmin = amin1(xi(1),xi(2))
      ymin = amin1(yi(1),yi(2))
      xmax = amax1(xi(1),xi(2))
      ymax = amax1(yi(1),yi(2))
      icheck = 0
      IF ( xint+.001.GE.xmin .AND. xint-.001.LE.xmax .AND. 
     &     yint+.001.GE.ymin .AND. yint-.001.LE.ymax ) THEN
         icheck = 1
      ELSEIF ( xint+tol.LT.xmin .OR. xint-tol.GT.xmax .OR. 
     &         yint+tol.LT.ymin .OR. yint-tol.GT.ymax ) THEN
         GOTO 200
      ENDIF
      xmin = amin1(xl(1),xl(2))
      ymin = amin1(yl(1),yl(2))
      xmax = amax1(xl(1),xl(2))
      ymax = amax1(yl(1),yl(2))
      IF ( icheck.NE.0 ) THEN
         IF ( xint+.001.GE.xmin .AND. xint-.001.LE.xmax .AND. 
     &        yint+.001.GE.ymin .AND. yint-.001.LE.ymax ) THEN
            iflag = 2
            RETURN
         ENDIF
      ENDIF
      IF ( xint+tol.GE.xmin .AND. xint-tol.LE.xmax .AND. 
     &     yint+tol.GE.ymin .AND. yint-tol.LE.ymax ) THEN
         iflag = 1
         RETURN
      ENDIF
C  CHECK TO SEE WHETHER ENDPOINTS OF LINES LIE WITHIN
C  GIVEN TOLERANCE
 200  CONTINUE
      IF ( abs(xi(1)-xl(1)).GT.tol .OR. abs(yi(1)-yl(1)).GT.tol ) THEN
         IF ( abs(xi(1)-xl(2)).GT.tol .OR. abs(yi(1)-yl(2)).GT.tol )
     &         THEN
            IF ( abs(xi(2)-xl(1)).GT.tol .OR. abs(yi(2)-yl(1)).GT.tol )
     &            THEN
               IF ( abs(xi(2)-xl(2)).GT.tol .OR. abs(yi(2)-yl(2))
     &              .GT.tol ) RETURN
            ENDIF
            xint = xi(2)
            yint = yi(2)
            iflag = 1
            RETURN
         ENDIF
      ENDIF
      xint = xi(1)
      yint = yi(1)
      iflag = 1
      RETURN
      END
