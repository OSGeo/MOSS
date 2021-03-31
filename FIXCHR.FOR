      SUBROUTINE fixchr(in,ndim)
      INTEGER in(*) , iuse(30)
      DO l = 1 , ndim/2
         iuse(l) = in(l)
      ENDDO
      CALL unpack(iuse,ndim)
      DO l = 1 , ndim
         IF ( iuse(l).EQ.0 ) iuse(l) = 32
      ENDDO
      CALL packc(iuse,in,ndim)
      RETURN
      END
