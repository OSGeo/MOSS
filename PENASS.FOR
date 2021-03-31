C***********************************************************************
C          NAME  = CONTAINS 10 CHARACTERS (5 WORDS) OF NAME FOR QUESTION
C          IPEN  = RETURNED PEN TYPE (0,1,2,3...8)
C***********************************************************************
      SUBROUTINE penass(name,ipen)
C        COMMON /CHANL/ IO(40)
      DIMENSION name(5)
C
 100  CONTINUE
      WRITE (*,99001) (name(k),k=1,5)
99001 FORMAT (' Enter pen position to plot the ',5A2,' [0,1,2,...8] '\)
      CALL readin(ipenn,1,is,ie)
      CALL inum(ipenn,ipen,is)
      IF ( ipen.GE.0 .AND. ipen.LE.8 ) RETURN
      WRITE (*,99002)
99002 FORMAT ('ILLEGAL PEN TYPE -- TRY AGAIN')
      GOTO 100
      END
