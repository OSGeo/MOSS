      SUBROUTINE poff(iarry,byte,ival,ipos)
c
c   put ival at the ipos (1 or 2) byte in iarry
c
c   ival <=256
c   ipos 1 ot 2
c
c ccccccccccccccccccccccccccccccccccccccccccccc
      INTEGER iarray(1) , iword(2) , byte
c
      ipos = 2
      jpos = byte/2
      IF ( mod(byte,2).NE.0 ) THEN
         ipos = 1
         jpos = jpos + 1
      ENDIF
      CALL byteit(iarray(jpos),iword)
c
      iword(ipos) = ival
c
c  put back together
c
      iarray(jpos) = iword(1)*256 + iword(2)
c
      RETURN
      END
