      SUBROUTINE circomp(xcirc,ycirc,ndim,radius)
C
C *****************************************************************************
C
C             S U B R O U T I N E     C I R C O M P
C FUNCTION:
C          THE FUNCTION OF CIRCOMP IS TO CALCULATE THE COORDINATES
C          OF A CIRCLE OF RADIUS 'RADIUS' ABOUT THE ORIGIN.
C PARAMETERS:
C             XCIR,YCIR   OUT- ARRAYS OF X AND Y COORDINATE CIRCLE
C                              PAIRS.
C             NDIM        IN - SIZE OF BOTH ARRAYS.
C             RADIUS      IN - RADIUS OF CIRCLE IN CM'S.
C *****************************************************************************
C
      DIMENSION xcirc(ndim) , ycirc(ndim)
      npq = ndim/4
C
C  DO THE FIRST QUADRANT
C
      xinc = radius/float(npq)
      xp = radius
C
      nnpq = npq - 1
      srad = radius*radius
      DO i = 1 , nnpq
         ypyp = srad - (xp*xp)
         yp = 0.0
         IF ( ypyp.GT.0 ) THEN
            yp = sqrt(srad-(xp*xp))
         ENDIF
         xcirc(i) = xp
         ycirc(i) = yp
         xp = xp - xinc
      ENDDO
      xcirc(npq) = 0.0
      ycirc(npq) = radius
C
C  SECOND QUADRANT
C
      nupper = npq + npq
      lower = npq + 1
      DO i = lower , nupper
         k = nupper - i + 1
         xcirc(i) = -xcirc(k)
         ycirc(i) = ycirc(k)
      ENDDO
C
C  THIRD QUADRANT
      nupper = nupper + npq
      lower = lower + npq
      ioff = npq*2
      DO i = lower , nupper
         k = i - ioff
         xcirc(i) = -xcirc(k)
         ycirc(i) = -ycirc(k)
      ENDDO
C
C  FORTH QUADRANT
C
      nupper = nupper + npq
      lower = lower + npq
      k = npq + 1
      DO i = lower , nupper
         k = k - 1
         xcirc(i) = xcirc(k)
         ycirc(i) = -ycirc(k)
      ENDDO
      RETURN
      END
