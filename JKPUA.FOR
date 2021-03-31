      SUBROUTINE jkpua(iunit,ir,x,m,iy,n)
C***
C***  THIS ROUTINE WRITES ONE STATPAC RECORD WITH THE QUALIFYING
C***  CODES PACKED. this version is used by ss2dgrid for the
c***  output of tempkrig.ugr.
c***
c*** modified: s.k. goodman, autometric, apr 88, pc moss
C***
      INTEGER*4 loc
      DIMENSION x(m) , iy(99) , loc(2)
      CHARACTER*4 chrid(4)
c      WRITE(*,*) 'JKPUA: IR,M,N,X,IY= ',IR,M,N, (X(IZ),IZ=1,M),
c     .  (IY(IZ),IZ=1,N)
      WRITE (iunit,ERR=100,IOSTAT=ier) ir , (x(i),i=1,m) , (iy(i),i=1,n)
      RETURN
 100  CONTINUE
      WRITE (*,*) '*** JKPUA ERROR =' , ier
      RETURN
      END
