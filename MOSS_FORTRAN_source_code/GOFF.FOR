      SUBROUTINE goff(ival,byte,iarray)
c
c   get ival at the ipos (1 or 2) byte in iarray
c
c   ival <=128
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
      ival = iword(ipos)
c
      RETURN
      END
