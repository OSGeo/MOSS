      SUBROUTINE b2sort(var1,var2,ipoint,n)
C***  CODED BY W. D. GRUNDY  MAY  1984
C***
C***  THIS SUBROUTINE IS REQUIRED BY PROGRAM STPSORT.
C***  IT SORTS THE MINOR SORT KEY AFTER THE SORT ON
C***  THE MAJOR KEY HAS BEEN FINISHED.
C***  THIS IS A BUBBLE SORT AND IS STABLE.
C***
      DIMENSION var1(1) , var2(1) , ipoint(1)
      DO i = 2 , n
         DO j = 2 , i
            jj = i - j + 1
            IF ( var1(ipoint(jj+1)).NE.var1(ipoint(jj)) ) GOTO 100
            IF ( var2(ipoint(jj+1)).GE.var2(ipoint(jj)) ) GOTO 100
            itemp = ipoint(jj)
            ipoint(jj) = ipoint(jj+1)
            ipoint(jj+1) = itemp
         ENDDO
 100  ENDDO
      RETURN
      END
