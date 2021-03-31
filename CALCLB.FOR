C***************************************************************************
C
C  Name:     C A L C L B
C
C  Purpose:  CALCULATE LABEL POSITIONING FOR TEXT MAPS, CONTOUR LABELING
C
C  Commons/Variables    In/Out        Attributes   Function/Size
C
C  -- Dummy Arguments --
C     NCHARS            In            Integer - Number of characters in label
C     HGT               In            Real    - Height of characters
C     THETA             In/Out        Real    - Initial/final angle of label
C     X1                In            Real    - First X coordinate
C     Y1                In            Real    - First Y coordinate
C     X2                In            Real    - Second X coordinate
C     Y2                In            Real    - Second Y coordinate
C     XLAB              Out           Real    - Begin label X coordinate
C     YLAB              Out           Real    - Begin label Y coordinate
C
C  -- Stack Variables --
C     DISX                            Real
C     DISY                            Real
C     DISL                            Real
C     DMIN                            Real
C
C  -- External Subprograms --
C     None
C
C  Files:
C       Unit    File name or description
C       ------  -------------------------------------------------------
C       None
C
C  Calling:          LBGENR
C
C  Notes:            None
C
C******************************************************************************
C
      SUBROUTINE calclb(nchars,hgt,theta,x1,y1,x2,y2,xlab,ylab)
C
C.....COMPUTE THE ANGLE THIS CONTOUR IS AT RELATIVE TO X-AXIS
      disx = x1 - x2
      disy = y1 - y2
      disl = (disx*disx) + (disy*disy)
      disl = sqrt(disl)
      dmin = float(nchars)*hgt
      IF ( abs(disx).LT.1.0E-05 ) THEN
         theta = acos(0.0)
         IF ( disy.GE.0.0 ) GOTO 100
      ELSE
         theta = atan(abs(disy/disx))
C
C.......MOD THETA AND XISCT,YLAB(THE LOWER LEFT CORNER OF THE FIRST
C.......SYMBOL TO BE WRITTEN) SO THAT THE CONTOUR IS CENTERED AND
C.......ORIENTED IN EITHER THE 1ST OR 4TH QUADRANTS
         IF ( disx.GE.0.0 .OR. disy.GE.0.0 ) THEN
            IF ( disx.GT.0.0 .AND. disy.GT.0.0 ) GOTO 100
            IF ( disx.GT.0.0 .AND. disy.LE.0.0 ) THEN
C
               xlab = x2
               ylab = y2
               theta = -theta
            ELSE
C
               xlab = x1
               ylab = y1
               theta = -theta
            ENDIF
            GOTO 200
         ENDIF
      ENDIF
C
      xlab = x1
      ylab = y1
      GOTO 200
C
 100  CONTINUE
      xlab = x2
      ylab = y2
C
 200  CONTINUE
      xlab = xlab + .025*cos(theta) + hgt*.5*sin(theta)
      ylab = ylab + .025*sin(theta) - hgt*.5*cos(theta)
      theta = theta*57.2958
C
      RETURN
      END
