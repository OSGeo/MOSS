      SUBROUTINE dsort(ds,ihol,n)
C***  THIS SUBROUTINE WAS WRITTEN BY KIM AND KNUDSEN
C***  THIS IS A SORTING ROUTINE IN ASCENDING ORDER
      DIMENSION ds(1) , ihol(1)
      dnew = ds(n)
      new = ihol(n)
      n1 = n - 1
      DO i = 1 , n1
         k = i
         IF ( dnew.LT.ds(i) ) THEN
            jk = 0
            DO i = k , n1
               j = n1 - jk
               jk = jk + 1
               ds(j+1) = ds(j)
               ihol(j+1) = ihol(j)
            ENDDO
            ds(k) = dnew
            ihol(k) = new
            GOTO 100
         ENDIF
      ENDDO
 100  CONTINUE
      RETURN
      END
