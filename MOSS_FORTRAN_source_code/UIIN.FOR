 
C*********************************************************************
C
C	NAME:		SUBROUTINE UIIN
C
C	FUNCTION:	ACCEPTS USER INPUT OF AN INTEGER IN CHARACTERS
C			FORM AND CONVERTS IT TO AN INTEGER WHILE
C			CHECKING FOR ERRORS
C
C	PARAMETERS:	NAME    USE    TYPE    DIMENSION    RANGE    UNITS
C			--------------------------------------------------
C			IIN      I     I*2         1
C 				THE INPUT CHANNEL TO READ FROM.
C                               ------------------------------------------
C			IOUT     I     I*2         1
C				THE OUTPUT CHANNEL TO WRITE TO.
C				--------------------------------------------
C			NUM      O     I*2         1        INTEGER
C				THE RETURNED INTEGER WHICH IS ZERO IF IC=0
C				--------------------------------------------
C			IC	 O     I*2         1        +INTEGER
C				THE NUMBER OF CHARACTERS INPUT. SET TO
C				ZERO IF CARRIAGE RETURN ONLY OR ALL
C				BLANKS IS INPUT
C
C				---------------------------------------------
C
C	FILES:
C
C	ROUTINES
C	REQUIRED:	ICHAR  DENUM  MOD
C
C	SYSTEM
C	DEPENDENCE:	NONE
C
C	LANGUAGE
C	DEPENDENCE:	FORTRAN
C
C*************************************************************************
C
      SUBROUTINE uiin(iin,iout,num,ic)
      DIMENSION iarr(20)
      DOUBLE PRECISION denum
      num = 0
C**     ACCEPT INPUT OF CHARACTERS AND CHECK FOR ERRORS
 100  CONTINUE
      ier = 0
      CALL readin(iarr,20,ic,istop)
      IF ( ic.NE.0 ) THEN
C**      COMPILE THE NUMBER,CALCULATE THE LENGTH, AND CHECK FOR ERRORS
         tnum = sngl(denum(iarr,ic,istop,ier))
         ic = istop - ic + 1
         IF ( ier.NE.0 ) THEN
C
            WRITE (iout,99001)
99001       FORMAT (' BAD CHARACTER...NUMBERS ONLY...PLEASE RE-ENTER')
            GOTO 100
C
C**     TOO MANY CHARACTERS INPUT
C
cssk 400    WRITE(IOUT,90400)
99002       FORMAT (/,
     &             ' TOO MANY CHARACTERS...MUST BE >=40 PLEASE RE-ENTER'
     &             )
C
C**     CHECK OUT THE NUMBER
C
         ELSEIF ( abs(tnum).GE.32768. ) THEN
            WRITE (iout,99003)
99003       FORMAT (
     &          ' NUMBER TOO LARGE...MUST BE LT 32768...PLEASE RE-ENTER'
     &          )
            GOTO 100
C
         ELSEIF ( amod(tnum,1.0).EQ.0 ) THEN
cssk       STOP
C
C**     LOOKS GOOD
C
            num = int(tnum)
         ELSE
            WRITE (iout,99004)
99004       FORMAT (
     &            ' FRACTIONAL NUMBER...INTEGERS ONLY...PLEASE RE-ENTER'
     &            )
            GOTO 100
C
C**     ERROR STOP
C
cssk 80000  WRITE(IOUT,98000) IER
99005       FORMAT (' *UIIN*   ERROR FROM ICHAR  ERROR',I5)
         ENDIF
      ENDIF
      RETURN
      END
