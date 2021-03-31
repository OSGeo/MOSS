      SUBROUTINE jkget(iunit,ir,chrid,loc,x,m,iy,n,iend,ierr)
C
C***
C***  THIS ROUTINE READS ONE STATPAC RECORD WITH THE QUALIFYING
C***  CODES PACKED.
C***
      INTEGER*2 loc(4)
      INTEGER*2 ir , iy , iix , iiy
      DIMENSION x(99) , iy(99)
      CHARACTER*4 chrid(4)
C
      iend = 0
C      write(*,*) 'jkget:'
C      write(*,*)IR,(CHRID(I),I=1,4),(LOC(I),I=1,4),
C     .  X(1),X(2),X(3),IY(1)
      READ (iunit,ERR=100,END=100) ir , (chrid(i),i=1,4) , 
     &                             (loc(i),i=1,4) , x(1) , x(2) , x(3) , 
     &                             iy(1)
      GOTO 200
 100  CONTINUE
      iend = 1
 200  CONTINUE
      RETURN
      END
