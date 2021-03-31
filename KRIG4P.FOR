      SUBROUTINE krig4p(b,ns,ier)
C***  THIS SUBROUTINE OBTAINS THE KRIGING WEIGHTS FOR POINT-KRIGING
C***  THIS SUBROUTINE IS A MODIFICATION OF ONE WRITTEN BY
C***  J.A. SKRIVAN AND M.A. KARLINGER OF THE USGS.
C***  SUBPROGRAMS REQUIRED: MAXCOR,PSET,COVAR4,TERM,DOLITL
      CHARACTER*8 iholid
C      CHARACTER*4 IVIB
      INTEGER*4 ivib
      INTEGER*2 ex , ey
$INCLUDE: 'MYST.INC'
      COMMON /cov4  / p(1830) , id(60) , w(60) , ihol(51) , r(60)
      DIMENSION b(2) , p1(2)
      ntot = ns + nt
C***  GET MAXIMUM COORDINATES OF DRILL HOLES FOR SCALING PURPOSES
C***  IF NT=1, THERE IS NO NEED FOR CALLING MAXCOR
      xmax = 0.
      ymax = 0.
      IF ( nt.NE.1 ) THEN
         CALL maxcor(xmax,ymax,ns)
      ENDIF
C***  SET UP LOWER TRIANGULAR MATRIX OF COVARIANCES AND DRIFT TERMS
      CALL pset4(ns,xmax,ymax)
C***  SET UP RIGHT HAND SIDE
      DO i = 1 , ns
         j = ihol(i)
         p1(1) = xe(j)
         p1(2) = yn(j)
         r(i) = covar4(b,p1)
      ENDDO
      k = ns + 1
      DO i = k , ntot
         m = i - ns
         r(i) = term(b(1),ex(m),b(2),ey(m),xmax,ymax)
      ENDDO
C***  GET WEIGHTS WHICH WILL BE STORED IN ARRAY W
      CALL dolitl(ntot,0,ier)
      RETURN
      END
