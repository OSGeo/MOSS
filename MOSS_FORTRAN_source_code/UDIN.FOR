C*********************************************************************
C
C	NAME:		SUBROUTINE UDIN
C
C	FUNCTION:	ACCEPTS USER INPUT OF A DOUBLE PRECISION NUMBER
C			IN CHARACTER FORM AND THEN CONVERTS IT TO A
C			DOUBLE PRECISION VALUE WHILE CHECKING FOR ERRORS
C
C	PARAMETERS:    	NAME    USE    TYPE    DIMENSION    RANGE    UNITS
C			---------------------------------------------------
C			IIN      I     I*2         1
C				THE INPUT CHANNEL TO READ FROM.
C				-------------------------------------------
C			IOUT     I     I*2         1
C				THE OUTPUT CHANNEL TO WRITE TO.
C				--------------------------------------------
C			DNUM     O     R*8         1         DOUBLE
C				THE RETURNED DOUBLE PRECISION NUMBER
C				WHICH IS ZERO IF IC=0
C				--------------------------------------------
C			IC       O     I*2         1       +INTEGER  BYTES
C				THE NUMBER OF CHARACTERS INPUT. SET TO ZERO
C				IF CARRIAGE RETURN ONLY OR ALL BLANKS IS
C				INPUT
C				--------------------------------------------
C
C	FILES:
C
C	ROUTINES
C	REQUIRED:DENUM
C
C	SYSTEM
C	DEPENDENCE:	NONE
C
C	LANGUAGE
C	DEPENDENCE:	FORTRAN
C
C*************************************************************************
C
      SUBROUTINE udin(iin,iout,dnum,ic)
      DOUBLE PRECISION denum , dnum , rnum
      DIMENSION iarr(20)
      dnum = 0.0
C**     ACCEPT INPUT OF THE CHARACTER STRING AND CHECK FOR ERRORS
 100  CONTINUE
      ier = 0
      CALL readin(iarr,20,ic,istop)
      IF ( ic.NE.0 ) THEN
C**      COMPILE THE NUMBER, CALCULATE THE LENGTH, AND COMPILE THE NUMBER
         rnum = denum(iarr,ic,istop,ier)
         IF ( ier.NE.0 ) THEN
C
            WRITE (iout,99001)
99001       FORMAT (' BAD CHARACTER...NUMBERS ONLY...PLEASE RE-ENTER')
            GOTO 100
C
C**     TOO MANY CHARACTERS INPUT
C
cssk 400    WRITE (IOUT,90400)
99002       FORMAT (/,
     &             ' TOO MANY CHARACTERS...MUST BE <=40 PLEASE RE-ENTER'
     &             )
C
C**     ERROR STOP
C
cssk 80000  WRITE(IOUT,98000) IER
99003       FORMAT (' *UDIN*   ERROR FROM readin  ERROR',I5)
         ELSE
            ic = istop - ic + 1
            dnum = rnum
         ENDIF
      ENDIF
cssk       STOP
C
C**    JUMP OUT
C
      RETURN
      END
