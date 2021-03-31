      SUBROUTINE maxcor(xmax,ymax,ns)
C***  COMPUTES MAXIMUM VALUES OF XE AND YN IN DATA SET
C***  THIS SUBROUTINE WAS WRITTEN BY W.D. GRUNDY, JANUARY, 1980
$INCLUDE: 'MYST.INC'
      CHARACTER*8 iholid
C      CHARACTER*4 IVIB
      INTEGER*4 ivib
      INTEGER*2 ex , ey
      COMMON /cov4  / p(1830) , id(60) , w(60) , ihol(51) , r(60)
      xmax = xe(ihol(1))
      ymax = yn(ihol(1))
      DO i = 2 , ns
         m = ihol(i)
         IF ( xmax.LT.xe(m) ) THEN
            xmax = xe(m)
         ENDIF
         IF ( ymax.LT.yn(m) ) THEN
            ymax = yn(m)
         ENDIF
      ENDDO
      RETURN
      END
