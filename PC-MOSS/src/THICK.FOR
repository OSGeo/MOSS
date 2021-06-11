      SUBROUTINE thick(xx,yy,ipen)
      REAL inipen , linang , lnang1 , lnang2
      COMMON /mdevice/ idev
      COMMON /thic  / dfltpn , ntim , pnwd
      COMMON /ththtt/ iflag , inipen , initim , pdown , pi , pup , x1 , 
     &                y1
      INTEGER pdown , pup
C
C  LINE THICK DRAWING
C
      IF ( pnwd.LE.0. ) pnwd = dfltpn
      x2 = xx
      y2 = yy
      ip = ipen
C
C  SINGLE LINE ?
C
      IF ( ntim.LE.1 ) GOTO 100
C
C  PEN DOWN MOVE ?
C
      IF ( ip.NE.2 ) THEN
C
C  PEN UP MOVE ?
C
         IF ( ip.EQ.3 ) GOTO 100
      ENDIF
C
C
      dx = x2 - x1
      dy = y2 - y1
      IF ( (dx.NE.0.0) .OR. (dy.NE.0.0) ) THEN
         IF ( dx.NE.0.0 ) THEN
C
            slope = dy/dx
            linang = atan(slope)
         ELSE
C
            linang = -pi/2.0
         ENDIF
C
         IF ( (dx.GT.0.0) .AND. (dy.GE.0.0) ) THEN
C
            lnang1 = linang
            lnang2 = linang + pi
         ELSEIF ( (dx.LE.0.0) .AND. (dy.GT.0.0) ) THEN
C
            lnang1 = linang + pi
            lnang2 = linang + (2.0*pi)
         ELSEIF ( (dx.LT.0.0) .AND. (dy.LE.0.0) ) THEN
C
            lnang1 = linang + pi
            lnang2 = linang
         ELSE
C
            lnang1 = linang + (2.0*pi)
            lnang2 = linang + pi
         ENDIF
C
         radius = 0.5*(ntim-1)*pnwd
C
         DO i = 1 , ntim
            s1 = 0.5*((2*i-ntim-1)*pnwd)
            s2 = -s1
C
            tang1 = asin(s1/radius)
            tang2 = asin(s2/radius)
            aang1 = (lnang1-pi) + tang1
            aang2 = (lnang2-pi) + tang2
C
            rotx1 = radius*cos(aang1)
            roty1 = radius*sin(aang1)
C
            rotx2 = radius*cos(aang2)
            roty2 = radius*sin(aang2)
C
            px1 = x1 + rotx1
            py1 = y1 + roty1
C
            px2 = x2 + rotx2
            py2 = y2 + roty2
C
            CALL plotdv(px1,py1,3,idev)
            CALL plotdv(px2,py2,2,idev)
         ENDDO
C
         ip = 3
      ENDIF
 100  CONTINUE
      x1 = x2
      y1 = y2
      CALL plotdv(x2,y2,ip,idev)
C
      RETURN
      END
