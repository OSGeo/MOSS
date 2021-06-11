      SUBROUTINE chrbld(istrng,cstrng)
C
C   routine to create a character filename from an integer
C     filename.  <istrng> must be dimensioned to atleast 25
C     for the pc version and 30 for the dg version
C
      CHARACTER cstrng*25
      INTEGER wrk(30) , istrng(*)
C
C   test if packed - if so unpack
C
      DO l = 1 , 25
         wrk(l) = istrng(l)
      ENDDO
      IF ( wrk(1).GT.255 ) CALL unpack(wrk,30)
C
C   convert name to character string
C
      cstrng = ' '
C
      DO l = 1 , 25
         IF ( wrk(l).EQ.32 .OR. wrk(l).EQ.0 ) GOTO 100
C  look for numbers
         IF ( wrk(l).GT.47 .AND. wrk(l).LT.58 ) cstrng(l:l)
     &         = char(wrk(l))
C  look for upper case alpha
         IF ( wrk(l).GT.64 .AND. wrk(l).LT.91 ) cstrng(l:l)
     &         = char(wrk(l))
C  look for lower case alpha
         IF ( wrk(l).GT.96 .AND. wrk(l).LT.123 ) cstrng(l:l)
     &         = char(wrk(l))
C  look for a dot
         IF ( wrk(l).EQ.46 ) cstrng(l:l) = char(wrk(l))
C  look for a back slash
         IF ( wrk(l).EQ.92 ) cstrng(l:l) = char(wrk(l))
      ENDDO
 
C
C
 100  CONTINUE
      RETURN
      END
