      SUBROUTINE receive(key,length)
C**************************************************************************
C
C NAME : SUBROUTINE RECEIVE
C
C FUNCTION : READ IN A CHARACTER STRING OF NO MORE THAN 10 CHARACTERS LONG
C
C PARAMETERS : KEY : ( ARRAY DEMENSION 10, OUTPUT ) WILL RETURN THE
C                        CHARACTER STRING
C              LENGTH : ( INTEGER, INPUT ) IS THE NUMBER OF CHARACTERS
C                             LONG THE KEY IS EXPECTED TO BE
C
C ROUTINES REQUIRED : SUBROUTINE READIN, SUBROUTINE DECIP
C
C**************************************************************************
C
$Include:'IO.inc'
C
      COMMON /string/ input(80) , inputp
      INTEGER key(80)
C
      CALL readin(input,80,inputp,istop)
      CALL decip(input,key,inputp)
      inputp = inputp - 1
C
C RETURN THE SPECIFIED NUMBER OF CHARACTERS( LENGTH )
C
      IF ( inputp.LT.length ) THEN
         istrt = inputp + 1
C
C IF THE CHARACTER STRING IS LESS THEN THE SPECIFIED NUMBER OF
C CHARACTERS THEN ZERO OUT THE UNUSED WORDDS.
C
         DO limit = istrt , length
            key(limit) = 0
         ENDDO
      ENDIF
C
      RETURN
      END
