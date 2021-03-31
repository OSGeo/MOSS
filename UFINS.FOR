      SUBROUTINE ufins(iin,iout,tnum,ndim,ic,ifirst)
C**********************************************************************
C
C     NAME:      SUBROUTINE UFINS
C
C     FUNCTION:  ACCEPTS USER INPUT OF A LINE OF NUMERIC CHARACTERS,
C                AND CONVERTS EACH DELIMITED STRING INTO AN ELEMENT
C                OF A FLOATING POINT ARRAY.  DELIMITERS MAY BE ONE
C                OR ANY COMBINATION OF THE FOLLOWING SYMBOLS:
C                " " (BLANK), "," (COMMA), "/" (SLASH), ":" (COLON),
C                OR ";" (SEMI-COLON).  IF A LINE CONTAINS FEWER
C                NUMERIC STRINGS THAN THE ARRAY DIMENSION SIZE, THE
C                REMAINING ELEMENTS ARE SET TO ZERO.
C
C    PARAMETERS:     NAME    USE    TYPE    DIMENSION    RANGE    UNITS
C                    __________________________________________________
C
C                    IIN     I      I*2
C                            THE INPUT CHANNEL TO READ FROM.
C                            -------------------------------------------
C                    IOUT    I      I*2
C                            THE OUTPUT CHANNEL TO WRITE TO.
C                            ------------------------------------------
C                    TNUM    O      R*4     NDIM         REAL
C                            THE RETURNED ARRAY OF FLOATING POINT
C                            NUMBERS, VALUE OF ZERO IF IC=0.
C                            ------------------------------------------
C                    NDIM    I      I*2                  +INT
C                            THE DIMENSION OF TNUM ARRAY.
C                            ------------------------------------------
C                    IC      0      I*2                  +INT     BYTES
C                            THE NUMBER OF CHARACTERS INPUT.  SET TO
C                            ZERO IF CARRIAGE RETURN ONLY, OR IF INPUT
C                            LINE IS ALL BLANKS.
C                            ------------------------------------------
C                    IFIRST  I      I*2                  0,1,27,77
C                            INDICATES SPECIAL FIRST CHARACTER
C                            0--NO CHECK FOR SPECIAL CHARACTER
C                            1--CHECK FOR SPECIAL CHARACTER
C                           27--FIRST CHARACTER IS ESCAPE
C                           77--FIRST CHARACTER IS 'M' (MOVE)
C                            ------------------------------------------
C
C     ROUTINES
C     REQUIRED:     DENUM ICHAR
C
C**********************************************************************
C
      INTEGER iarr(132) , ndim , ibegn , iend , ier , knt , isymb , 
     &        istrt , istop , ifirs
      DOUBLE PRECISION denum
      REAL tnum(ndim)
      LOGICAL found
      DATA nmax/132/
C
C
 100  CONTINUE
      ier = 1
      DO i = 1 , ndim
         tnum(i) = 0.0
      ENDDO
C
C......READ ARRAY, ESTABLISH BEGINNING/END POSITION OF NON-BLANKS ......
C
      CALL readin(iarr,132,ibegn,iend)
      IF ( ibegn.EQ.132 ) ibegn = 0
C......CHECK FOR A BLANK LINE .......
      ic = 0
      IF ( ibegn.EQ.0 ) GOTO 400
C......SET NUMBER OF CHARACTERS ......
      ic = iend - ibegn + 1
C
C......CHECK SPECIAL CHARACTER FLAG .......
C
      IF ( ifirst.NE.0 ) THEN
         isymb = iarr(ibegn)
C...........TEST FOR ESCAPE CHARACTER .......
         IF ( isymb.EQ.27 ) THEN
            ifirst = 27
            ibegn = ibegn + 1
            IF ( ic.EQ.1 ) GOTO 400
C...........TEST FOR 'M' CHARACTER .......
         ELSEIF ( isymb.EQ.77 ) THEN
            ifirst = 77
            ibegn = ibegn + 1
            IF ( ic.EQ.1 ) THEN
C
               WRITE (iout,99001)
99001          FORMAT (/,
     &                 ' A NUMBER MUST FOLLOW (M) ... PLEASE RE-ENTER.')
               GOTO 100
            ENDIF
         ENDIF
      ENDIF
C
      found = .FALSE.
      knt = 0
C
C......PROCESS EACH CHARACTER IN ARRAY ......
C
      DO i = ibegn , iend
C...........GET NEXT CHARACTER ......
         isymb = iarr(i)
C...........CHECK FOR DELIMITERS  (" ", ",", "/", ":", ";") .......
         IF ( (isymb.EQ.32) .OR. (isymb.EQ.44) .OR. (isymb.EQ.47) .OR. 
     &        (isymb.EQ.58) .OR. (isymb.EQ.59) ) THEN
C...........CHARACTER IS DELIMITER .......
            IF ( found ) THEN
C................CHARACTER TERMINATES A NUMERIC STRING ......
               istop = i - 1
               knt = knt + 1
               IF ( knt.GT.ndim ) GOTO 200
C................CONVERT NUMERIC CHARACTERS TO A REAL VALUE ......
               tnum(knt) = sngl(denum(iarr,istrt,istop,ier))
               IF ( ier.NE.1 ) GOTO 300
               found = .FALSE.
            ENDIF
C................CHARACTER IS NUMERIC .......
         ELSEIF ( .NOT.(found) ) THEN
C.....................CHARACTER BEGINS A NUMERIC STRING .......
            istrt = i
            found = .TRUE.
         ENDIF
      ENDDO
C
C......CHECK FOR UNTERMINATED STRING ......
C
      IF ( .NOT.found ) GOTO 400
      istop = iend
      knt = knt + 1
      IF ( knt.LE.ndim ) THEN
         tnum(knt) = denum(iarr,istrt,istop,ier)
         IF ( ier.EQ.0 ) GOTO 400
         GOTO 300
C
C......ERROR RETURNS ......
C
cssk 85000  WRITE (IOUT,90050)
99002    FORMAT (/,'TOO MANY CHARACTERS--MUST BE LE 132,',
     &           ' PLEASE RE-ENTER.')
C
cssk 85020  WRITE(IOUT,98502) IER
99003    FORMAT (' *UFINS*  ERROR FROM ICHAR  ERROR',I5)
      ENDIF
cssk       STOP
C
 200  CONTINUE
      WRITE (iout,99004) ndim
99004 FORMAT (/,' ENTERED TOO MANY NUMBERS PER LINE.',/,
     &        'MAXIMUM SPECIFIED IS ',I3,'  PLEASE RE-ENTER.')
      GOTO 100
C
 300  CONTINUE
      WRITE (iout,99005)
99005 FORMAT (/,' BAD CHARACTER...NUMBERS ONLY...PLEASE RE-ENTER.')
      GOTO 100
C
 400  CONTINUE
      RETURN
      END
