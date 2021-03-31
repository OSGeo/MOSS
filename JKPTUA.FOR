      SUBROUTINE jkptua(iunit,ir,x,chia,m)
C***
C***  THIS ROUTINE WRITES  STATPAC RECORDs WITH THE QUALIFYING
C***  CODES PACKED. this version is used by ss2dgrid for the
c***  output of tempkrig.ugr.
c***
c*** modified: s.k. goodman, autometric, apr 88, pc moss
C***
C***
      INTEGER*4 loc
      DIMENSION loc(2) , x(m) , iy(99)
      CHARACTER*4 chrid(4)
      CHARACTER*1 chia(m)
      CALL jkpack(chia,m,iy,n)
      CALL jkpua(iunit,ir,x,m,iy,n)
      RETURN
      END
