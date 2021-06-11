      SUBROUTINE drftrm(iflag)
C***  THIS SUBROUTINE INPUTS THE DRIFT TERMS
C***  THIS SUBROUTINE WAS WRITTEN BY W.D. GRUNDY, JANUARY, 1981
C***
C**********************************************************************
C***
C***                REMARKS ON ENTRY OF DRIFT TERMS
C***
C***  DRIFT TERMS USED IN UNIVERSAL KRIGING ARE REPRESENTED BY A SERIES
C***  OF MONOMIAL TERMS INVOLVING EXPONENTS OF THE X- AND Y-COORDINATES.
C***  FOR EXAMPLE, TO REPRESENT A LINEAR DRIFT OF THE FOLLOWING
C***  GENERAL FORM:
C***  Z = A + B*X +C*Y
C***  THE X- AND Y- EXPONENTS OF THE CONSTANT TERM A ARE BOTH ENTERED
C***  AS 0. THE NEXT TERM HAS AN X-EXPONENT OF 1 AND A Y-EXPONENT OF 0.
C***  THE THIRD TERM HAS AN X-EXPONENT OF 0 AND A Y-EXPONENT OF 1.
C***  IN UNIVERSAL KRIGING, THERE IS NO NEED TO KNOW THE ACTUAL VALUES
C***  OF THE COEFFICIENTS A, B, AND C.
C***  THIS SUBROUTINE DOES NOT ALLOW THE ENTRY OF NEGATIVE OR NON-
C***  INTEGER EXPONENTS.   ENTRIES WITH DECIMAL POINTS WILL NOT BE
C***  ACCEPTED.
C***
C**********************************************************************
$INCLUDE: 'MYST.INC'
      CHARACTER*8 iholid
C      CHARACTER*4 IVIB
      INTEGER*4 ivib
      INTEGER*2 ex , ey
      COMMON /cov4  / p(1830) , id(60) , w(60) , ihol(51) , r(60)
      IF ( iflag.GT.0 ) GOTO 200
 100  CONTINUE
      ier = 1
      WRITE (*,99001)
99001 FORMAT (' DO YOU WANT 1) ORDINARY OR 2) ','UNIVERSAL KRIGING? ',/,
     &        ' ANSWER 1 OR 2 :',/,' ?'\)
      READ (*,99002,ERR=800) ians
99002 FORMAT (BN,I2)
      IF ( ians.LT.1 .OR. ians.GT.2 ) THEN
         WRITE (*,*) char(bel)
         WRITE (*,99003)
99003    FORMAT (' YOU MUST TYPE 1 OR 2')
         GOTO 100
      ELSEIF ( ians.EQ.1 ) THEN
C***  ORDINARY KRIGING IS WANTED
         nt = 1
         ex(1) = 0
         ey(1) = 0
         GOTO 700
      ENDIF
C***  UNIVERSAL KRIGING IS WANTED
 200  CONTINUE
      WRITE (*,99004)
99004 FORMAT (/,' ENTER DRIFT TERM EXPONENTS')
 300  CONTINUE
      ier = 2
      WRITE (*,99005)
99005 FORMAT (' ENTER NUMBER OF MONOMIALS (MAX=10):',/,' ?'\)
      READ (*,99002,ERR=800) nt
      IF ( nt.LT.1 .OR. nt.GT.10 ) THEN
         WRITE (*,*) char(bel)
         WRITE (*,99006)
99006    FORMAT (' YOU MUST ENTER A NUMBER FROM 1 TO 10')
         GOTO 300
      ENDIF
 400  CONTINUE
      WRITE (*,99007)
99007 FORMAT (/,' REMEMBER THAT THE X- AND Y-EXPONENTS OF THE',
     &        ' FIRST (CONSTANT) TERM',/,' ARE BOTH ENTERED AS',
     &        ' ZEROES.')
      i = 1
 500  CONTINUE
      ier = 3
      WRITE (*,99008) i
99008 FORMAT (' ENTER THE X-EXPONENT OF TERM ',I2,':',/,' ?'\)
      READ (*,99002,ERR=800) ex(i)
 600  CONTINUE
      ier = 4
      WRITE (*,99009) i
99009 FORMAT (' ENTER THE Y-EXPONENT OF TERM ',I2,':',/,' ?'\)
      READ (*,99002,ERR=800) ey(i)
      IF ( ex(i).LT.0 .OR. ey(i).LT.0 ) THEN
         WRITE (*,*) char(bel)
         WRITE (*,99010)
99010    FORMAT (' THE EXPONENTS MUST NOT BE NEGATIVE')
         GOTO 500
      ELSEIF ( i.LE.1 ) THEN
         IF ( ex(1).NE.0 .OR. ey(1).NE.0 ) THEN
            WRITE (*,*) char(bel)
            GOTO 400
         ENDIF
      ENDIF
      i = i + 1
      IF ( i.LE.nt ) GOTO 500
 700  CONTINUE
      RETURN
 800  CONTINUE
      WRITE (*,*) char(bel)
      WRITE (*,99011)
99011 FORMAT (' INTEGER VALUE WAS EXPECTED; REENTER VALUE')
      IF ( ier.EQ.1 ) GOTO 100
      IF ( ier.EQ.2 ) GOTO 300
      IF ( ier.EQ.3 ) GOTO 500
      IF ( ier.EQ.4 ) GOTO 600
      END
