      SUBROUTINE blfill(icard,k)
      DIMENSION icard(k)
C
C  FUNCTION: CONVERT ALL 0 VALUES TO BLANKS
C
C  PROGRAMMED BY CARL REED, AUTOMETRIC INC., MAY 1983
C
      DO i = 1 , k
         IF ( icard(i).LE.0 ) icard(i) = 32
C
      ENDDO
C
      RETURN
      END
