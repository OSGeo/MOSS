      SUBROUTINE callab(hgt,isct,nchars,npts,x1,y1,x2,y2)
CC
      DIMENSION isct(1)
C
C     COMPUTE THE ANGLE THIS CONTOUR IS AT RELATIVE TO X-AXIS
C
      disx = x1 - x2
      disy = y1 - y2
      disl = disx*disx + disy*disy
      disl = sqrt(disl)
      dmin = float(nchars)*hgt
C      IF(DISL.LT.DMIN) RETURN
      IF ( abs(disx).LT.1.E-05 ) THEN
         theta = acos(0.0)
         IF ( disy.GE.0.0 ) GOTO 100
      ELSE
         theta = atan(abs(disy/disx))
C
C        MOD THETA AND XISCT,YLAB(THE LOWER LEFT CORNER OF THE FIRST
C             SYMBOL TO BE WRITTEN) SO THAT THE CONTOUR IS CENTERED AND
C             ORIENTED IN EITHER THE 1ST OR 4TH QUADRANTS
C
         IF ( disx.GE.0.0 .OR. disy.GE.0.0 ) THEN
            IF ( disx.GT.0.0 .AND. disy.GT.0.0 ) GOTO 100
            IF ( disx.GT.0.0 .AND. disy.LE.0.0 ) THEN
               xlab = x2
               theta = -theta
               ylab = y2
            ELSE
               theta = -theta
               xlab = x1
               ylab = y1
            ENDIF
            GOTO 200
         ENDIF
      ENDIF
      xlab = x1
      ylab = y1
      GOTO 200
 100  CONTINUE
      xlab = x2
      ylab = y2
 200  CONTINUE
      xlab = xlab + .025*cos(theta) + hgt*.5*sin(theta)
      ylab = ylab + .025*sin(theta) - hgt*.5*cos(theta)
      theta = theta*57.2958
      CALL plotsym(0,15,xlab,ylab,hgt,hgt,isct,0.0,0.0,theta,0,nchars,
     &             ier)
      RETURN
      END
