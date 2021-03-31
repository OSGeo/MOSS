 
      SUBROUTINE udconv(iconv,nconv,dist,dist1,ier)
C
      COMMON /udcovrt/ rmet(8) , feet(8)
      COMMON /io    / nprnt , ioin
C
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
C
C                S U B R O U T I N E    U D C O N V
C
C  FUNCTION:  GIVEN UNITS OF MEASUREMENT AND A DISTANCE IN THOSE
C             UNITS, CALCULATE THE SAME MEASUREMENT IN FEET, METERS,
C             AND MILES
C
C  INPUT PARAMETERS:
C
C             ICONV = UNITS OF MEASURE
C                     1=METERS
C                     2=FEET
C                     3=INCHES
C
C             DIST  = DISTANCE IN UNITS OF MEASURE
C             NCONV  = UNITS OF MEASURE FOR RETURNED DISTANCE
C                     1=MILES
C                     2=KILOMETERS
C                     3=METERS
C                     4=FEET
C                     5=INCHES
C                     6=NAUTICAL MILES
C                     7=YARDS
C                     8=RODS
C
C             DIST1 = RETURNED MEASUREMENT IN IN DESIRED UNITS
C
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
      ier = 1
      IF ( nconv.LT.0 .OR. nconv.GT.7 ) THEN
         WRITE (nprnt,99001)
99001    FORMAT (' *UDCONV* Illegal units of measure - NCONV')
         RETURN
      ELSEIF ( iconv.LT.0 .OR. iconv.GT.3 ) THEN
C
C  ERROR PROCESSING
C
         WRITE (nprnt,99002)
99002    FORMAT (' *UDCONV* Illegal units of measure - ICONV')
         RETURN
      ELSE
C
         ier = 0
C
         dist1 = 0.0
         dist2 = 0.0
         dist3 = 0.0
         IF ( dist.NE.0.0 ) THEN
C
C  CHECK ICONV AND JUMP TO PROPER SECTION OF CODE
C
            IF ( iconv.EQ.2 ) THEN
C
C UNITS ARE FEET
C
               dist1 = dist/feet(nconv)
            ELSEIF ( iconv.NE.3 ) THEN
C
C  UNITS ARE METERS
C
               dist1 = dist/rmet(nconv)
            ENDIF
         ENDIF
C
C  UNITS ARE INCHES
C
C
C
C
         RETURN
      ENDIF
C
      END
