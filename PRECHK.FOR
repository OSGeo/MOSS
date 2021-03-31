 
      SUBROUTINE prechk(yn,xe,nhol)
C***  THIS SUBROUTINE IS USED BY SS2DGRID AND BY SS2DDRES.  IT
C***  CHECKS THE NORTH COORDINATES FOR ARRANGEMENT IN A SORTED
C***  ASCENDING ORDER.
C***
      DIMENSION yn(1) , xe(1)
      DO i = 2 , nhol
         IF ( yn(i).LT.yn(i-1) ) THEN
            WRITE (*,*) 'INPUT FILE WAS NOT PREPROCESSED'
            RETURN
         ENDIF
      ENDDO
      RETURN
      END
