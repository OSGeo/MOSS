      SUBROUTINE jkptls(iunit,ir,chrid,loc,x,chia,m)
C***
C***  THIS ROUTINE PACKS THE QUALIFYING CODES AND WRITES THE ONE
C***  STATPAC RECORD.
C***
      INTEGER*4 loc
      DIMENSION loc(2) , x(m) , iy(99)
      CHARACTER*4 chrid(4)
      CHARACTER*1 chia(m)
      CALL jkpack(chia,m,iy,n)
      CALL jkput(iunit,ir,chrid,loc,x,m,iy,n)
      RETURN
      END
