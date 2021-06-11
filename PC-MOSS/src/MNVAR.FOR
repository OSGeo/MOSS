      SUBROUTINE mnvar(ns,blkvar,gr,varkg)
C***  THIS SUBROUTINE COMPUTES THE KRIGING MEAN AND VARIANCE
C***  OF A BLOCK OF GROUND OR OF A POINT.
C***  IF CALL IS FROM SS2DXVAL OR SS2DGRID, THE VARIABLE BLKVAR IN THIS
C***  SUBROUTINE REPRESENTS THE VARIOGRAM SILL VALUE.
C***  THIS SUBROUTINE WAS WRITTEN BY W.D. GRUNDY, JANUARY, 1980
$INCLUDE: 'MYST.INC'
      CHARACTER*8 iholid
C      CHARACTER*4 IVIB
      INTEGER*4 ivib
      INTEGER*2 ex , ey
      COMMON /cov4  / p(1830) , id(60) , w(60) , ihol(51) , r(60)
C***  KRIGED VALUE WILL BE STORED IN GR
      ag = 0.
      gr = 0.
      ersum = 0.
      DO l = 1 , ns
         ll = ihol(l)
         ag = ag + r(l)*w(l)
         gr = gr + gd(ll)*w(l)
C     IF(ERMEAS(LL)) 10,20,10
C  10 ERSUM=ERSUM+ERMEAS(LL)*W(L)**2
      ENDDO
C***  GET PART OF KRIGING VARIANCE DUE TO DRIFT
      drfsum = 0.
      ns1 = ns + 1
      ntot = ns + nt
      DO l = ns1 , ntot
         drfsum = drfsum + r(l)*w(l)
      ENDDO
C***  KRIGING VARIANCE IS :
      varkg = blkvar - ag - drfsum + ersum
      RETURN
      END
