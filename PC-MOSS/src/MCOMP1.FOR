      LOGICAL FUNCTION mcomp1(string,templat,nchars,ier)
C**************************************************************
C  FUNCTION - MOSS STRING COMPARE   (INCOMING IS PACKED)
C *************************************************************
      INTEGER string(*) , wrk(80)
      CHARACTER*20 templat
C
      DO l = 1 , nchars
         wrk(l) = string(l)
      ENDDO
      CALL unpack(wrk,nchars)
      mcomp1 = .TRUE.
C
      DO i = 1 , nchars
         IF ( wrk(i).NE.ichar(templat(i:i)) ) mcomp1 = .FALSE.
      ENDDO
C
      RETURN
      END
