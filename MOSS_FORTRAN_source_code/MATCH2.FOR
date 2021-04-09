      SUBROUTINE match2(iar1,iar2,nword,kword,iflag)
C *********************************************************
C
C         ROUTINE   SMATCH
C
C     CHECK FOR MATCH OF TWO CHARACTER STRINGS
C
C  INPUT PARAMTERS-
C
C     (IAR1,IARR2) - ARRAYS CONTAINING TWO CHARACTER STRINGS
C                     TO COMPARE
C     NWORD         - NUMBER OF CHARACTERS IN IARR1
C     KWORD         - NUMBER OF CHARACTERS IN IARR2
C     IFLAG         - 0 IF NO MATCH, 1 IF MATCH
C
C ************************************************************
C
      DIMENSION iar1(nword) , iar2(kword)
      LOGICAL mcomp
      iflag = 0
      jword = kword - nword + 1
      DO i = 1 , jword
         IF ( .NOT.(mcomp(iar1(1),'+',1,ier)) ) THEN
            IF ( iar2(i).NE.iar1(1) ) GOTO 100
         ENDIF
         mn = i - 1
         DO kn = 1 , nword
            mn = mn + 1
            IF ( mn.GT.kword ) RETURN
            IF ( .NOT.(mcomp(iar1(kn),'+',1,ier)) ) THEN
               IF ( iar1(kn).NE.iar2(mn) ) GOTO 100
            ENDIF
         ENDDO
         iflag = 1
         GOTO 200
 100  ENDDO
C
 200  CONTINUE
      RETURN
      END
