      SUBROUTINE amove(nwords,from,to)
C
C****************************************************************
C
C     TITLE AMOVE
C
C     CALL FORMAT
C
C          CALL AMOVE(NWORDS,FROM,TO)
C
C*****************************************************************
C
      INTEGER*2 nwords
      INTEGER*2 from(nwords) , to(nwords)
C
      DO l = 1 , nwords
         to(l) = from(l)
      ENDDO
      RETURN
      END
