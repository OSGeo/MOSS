      SUBROUTINE jkunpk(chia,m,iy,n)
C***
C***  THIS ROUTINE UNPACKS THE QUALIFYING CODES
C***  CONTAINED IN ARRAY IY INTO ARRAY CHIA.
C***  ARGUMENTS THE SAME AS IN SUBROUTINE JKPACK.
C***
      DIMENSION iy(99)
      CHARACTER*1 chia(m) , ic(8)
      DATA ic/' ' , 'B' , 'L' , 'N' , 'G' , 'T' , 'H' , '*'/
      DO i = 1 , n
         n1 = 5
         IF ( i.EQ.n ) THEN
            n1 = 5*(1-n) + m
         ENDIF
         DO j = 1 , n1
            kx = mod(iy(i),8) + 1
            chia(5*(i-1)+(n1+1-j)) = ic(kx)
            iy(i) = iy(i)/8
         ENDDO
      ENDDO
      RETURN
      END
