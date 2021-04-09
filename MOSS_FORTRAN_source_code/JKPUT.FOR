      SUBROUTINE jkput(iunit,ir,chrid,loc,x,m,iy,n)
C   MODIFIED:  S.K. GOODMAN, AUTOMETRIC, APR 88 PC-MESS
C***
C***  THIS ROUTINE WRITES ONE STATPAC RECORD WITH THE QUALIFYING
C***  CODES PACKED.
C***
      INTEGER*2 loc(4)
      DIMENSION x(99) , iy(99)
      CHARACTER*4 chrid(4)
C
      WRITE (iunit) ir , (chrid(i),i=1,4) , (loc(i),i=1,4) , x(1) , 
     &              x(2) , x(3) , iy(1)
C      write(*,*) 'jkput:'
C      write(*,*)IR,(CHRID(I),I=1,4),(LOC(I),I=1,4),
C     .  X(1),X(2),X(3),IY(1)
      RETURN
      END
