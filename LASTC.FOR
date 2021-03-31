      SUBROUTINE lastc(iarray,last)
c
c   finds the last character of an array
c
c   input : last = number of words in iarray
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      INTEGER iarray(1) , iword(2)
c
      in = last
      last = last*2
c
      DO l = in , 1 , -1
         CALL byteit(iarray(l),iword)
         DO k = 1 , 2
            IF ( iword(k).NE.32 .AND. iword(k).NE.0 ) GOTO 100
            last = last - 1
         ENDDO
      ENDDO
c
 100  CONTINUE
      RETURN
      END
