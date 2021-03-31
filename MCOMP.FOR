      LOGICAL FUNCTION mcomp(string,templat,nchars,ier)
      INTEGER string(nchars)
      CHARACTER*(*) templat
C
C  FUNCTION - MOSS STRING COMPARE
      ier = 0
C
      mcomp = .TRUE.
C
      DO i = 1 , nchars
         IF ( string(i).NE.ichar(templat(i:i)) ) mcomp = .FALSE.
      ENDDO
C
      RETURN
      END
