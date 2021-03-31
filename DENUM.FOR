C*********************************************************************
C
C       NAME:           FUNCTION DENUM
C
C       FUNCTION:       DECODES A CHARACTER STRING AND RETURNS A NUMBER
C                       IF NO NON-NUMERIC CHARACTERS ARE FOUND (0-9,+,-,.)
C
C       PARAMETERS:     NAME    USE    TYPE    DIMENSION    RANGE    UNITS
C                       ---------------------------------------------------
C
C                       IARR     I     I*2       USER
C                               THE FIRST WORD ADDRESS OF THE ARRAY
C                               CONTAINING THE CHARACTER STRING.
C                               ----------------------------------------
C                       ISTRT    I     I*2         1        +INTEGER
C                               THE POSITION RELATIVE TO THE FWA OF THE
C                               FIRST CHARACTER.
C                               ----------------------------------------
C                       ISTOP    I     I*2         1        +INTEGER
C                               THE POSITION RELATIVE TO THE FWA OF THE
C                               LAST CHARACTER.
C                               ----------------------------------------
C                       IER      O     I*2         1           0,1
C                               THE ERROR FLAG.  SET TO ONE FOR NO ERROR.
C                               SET TO ZERO IF A NON-NUMERIC CHARACTER
C                               OR MORE THAN ONE DECIMAL POINT IS INPUT.
C                               ----------------------------------------
C
C     ROUTINES REQUIRED:  BYTE
C
C     CALLING ROUTINES:   RENUM
C
C     SYSTEM DEPENDENCE:  ASSUMES ASCII CHARACTER CODE
C
C     LANGUAGE DEPENDENCE:  FORTRAN
C
C
C     REFERENCES:  MOSS FNUM ROUTINE
C
C***********************************************************************
C
C
      DOUBLE PRECISION FUNCTION denum(iarr,istrt,istop,ier)
      DIMENSION iarr(*)
      DOUBLE PRECISION rnum , tkk , ten
      tkk = 1D0
      rnum = 0D0
      ten = 10D0
      denum = 0.0
      sign = 1.
      iper = 0
      ier = 0
C
      DO i = istrt , istop
         j = iarr(i)
         IF ( j.LT.43 .OR. j.GT.57 .OR. j.EQ.44 .OR. j.EQ.47 ) THEN
            ier = 1
            GOTO 100
         ELSEIF ( j.NE.45 .AND. j.NE.43 ) THEN
            IF ( j.EQ.46 ) iper = iper + 1
            IF ( iper.EQ.1 ) tkk = tkk*ten
            IF ( tkk.NE.ten ) THEN
               rnum = (rnum*ten) + dble(j-48)
            ENDIF
         ELSE
            IF ( j.EQ.45 ) sign = -1
         ENDIF
      ENDDO
C......MORE THAN ONE DECIMAL POINT WAS INPUT ?
      IF ( iper.GT.1 ) THEN
         ier = 1
      ELSE
         IF ( tkk.GT.1D0 ) tkk = tkk/ten
         denum = (rnum/tkk)*sign
      ENDIF
C
 100  CONTINUE
      RETURN
      END
