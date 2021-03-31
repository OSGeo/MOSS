      LOGICAL FUNCTION mcomph(string,templat,nchars,ier)
      INTEGER string(nchars)
      INTEGER templat(*)
C
C  FUNCTION - MOSS STRING COMPARE
C
C  PROGRAMMED BY CARL REED, AUTOMETRIC INC, JUNE 1982
      ier = 0
C
      mcomph = .TRUE.
C
      DO i = 1 , nchars
         IF ( string(i).NE.templat(i) ) mcomph = .FALSE.
      ENDDO
C
      RETURN
      END
