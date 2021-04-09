      SUBROUTINE gtb(ival,word,ipos)
c
c   get ival at the ipos (1 or 2) byte in iword
c
c   ival <=256
c   ipos 1 ot 2
c
c ccccccccccccccccccccccccccccccccccccccccccccc
      INTEGER word , iword(2)
c
      CALL byteit(word,iword)
c
      ival = iword(ipos)
c
      RETURN
      END
