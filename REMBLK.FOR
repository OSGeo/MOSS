      SUBROUTINE remblk(id)
      CHARACTER*1 id(8)
      DO i = 1 , 8
         IF ( id(i).EQ.' ' ) id(i) = '0'
      ENDDO
      RETURN
      END
