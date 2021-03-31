C*********************************************************************
C       NAME:           SUBROUTINE FRSTC
C
C       FUNCTION:       SEARCHES FOR THE FIRST NON-BLANK CHARACTER
C                       IN A STRING.
C
C       PARAMETERS:     NAME    USE    TYPE    DIMENSION    RANGE    UNITS
C                       --------------------------------------------------
C                       IOK      O     I*2        1          T,F
C                               LOGICAL VARIABLE SET TO .FALSE. IF
C                               PARAMETERS IN ERROR. IOK=.TRUE. OTHERWISE
C                               ----------------------------------------
C                       IARR     I     I*2       USER
C                               THE FIRST WORD ADDRESS OF THE ARRAY
C                               CONTAINING THE CHARACTER STRING
C                               ----------------------------------------
C                       ISTRT    I     I*2        1         +INTEGER
C                               THE CHARACTER POSITION RELATIVE TO THE
C                               FWA TO BEGIN SEARCHING FOR A CHARACTER
C                               ----------------------------------------
C                       NMAX     I     I*2        1         +INTEGER
C                               THE CHARACTER POSITION RELATIVE TO THE
C                               FWA TO STOP LOOKING FOR A NON-BLANK
C                               CHARACTER
C                               ----------------------------------------
C                       IC       O     I*2        1         +INTEGER
C                               THE CHARACTER POSITION RELATIVE TO THE
C                               FWA WHERE THE FIRST NON-BLANK CHARACTER
C                               WAS FOUND. SET TO NMAX+1 IF NO CHAR-
C                               ACTER WAS FOUND
C                               ----------------------------------------
C
C       FILES:
C
C       ROUTINES
C       REQUIRED:       gtb
C
C*********************************************************************
C
      SUBROUTINE frstc(iok,iarr,istrt,nmax,ic)
      DIMENSION iarr(1)
      LOGICAL iok
      iok = .TRUE.
C**      CHECK FOR PARAMETER CORRECTNESS
      IF ( istrt.GT.nmax ) iok = .FALSE.
      DO ic = istrt , nmax
         CALL goff(j,ic,iarr(1))
C**         CHECK FOR NULL WHICH IS STRING TERMINATOR
         IF ( j.EQ.0 ) GOTO 100
C**      CHECK FOR A NON-BLANK CHARACTER
         IF ( j.NE.32 ) GOTO 200
      ENDDO
C**      ERROR...NO NON-BLANK CHARACTERS FOUND OR INPUT STRING IS NULL
 100  CONTINUE
      ic = nmax + 1
 200  CONTINUE
      RETURN
      END
