C
C THIS SUBROUTINE REQUEST THE USER TO ENTER THE PARAMETERS
C FOR LINE TYPES THAT HAVE A WIDTH.
C
      SUBROUTINE dlindef(lstype,ispen,widthl,dmark)
C
C        COMMON /CHANL/ IO(40)
$Include:'strng.inc'
C        COMMON /STRNG/ ICARD(80),K
C
      IF ( lstype.GT.6 ) THEN
C/*  THESE LINES DON'T NEED 2 PENS
         WRITE (*,*) 'Some special lines require 2 pens.'
         CALL penass('2nd PEN   ',ispen)
      ENDIF
      IF ( lstype.LE.5 ) RETURN
C/*  NOT WIDE LINES
      WRITE (*,99001)
99001 FORMAT (' Enter LINE WIDTH, DIST between MARKERS (inches)',
     &        ' [CR=.1,.5] = '\)
      CALL readin(icard,lenicd,k,ie)
      CALL fnum(icard,80,widthl,k)
      CALL fnum(icard,80,dmark,k)
      IF ( widthl.LE.0 .OR. widthl.GT..5 ) widthl = .1
      IF ( dmark.LE.0 .OR. dmark.GT.3. ) dmark = .5
      RETURN
      END
