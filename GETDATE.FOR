      SUBROUTINE getdate(id)
C***  PLACES CURRENT DATE IN MO/DA/YR IN CHARACTER*8 VARIABLE
C***  CODED BY W.D. GRUNDY  JANUARY 1985
C***
      CHARACTER*8 id
      CHARACTER*1 slash
      INTEGER*2 mo , iday , iyr
      DATA slash/'/'/
      CALL mdyhms(mo,iday,iyr,ihr,min,isec,ih100)
CSKG      CALL GETDAT(IYR,MO,IDAY)
      iyr = iyr - 1900
      WRITE (id,99001,ERR=100) mo , slash , iday , slash , iyr
99001 FORMAT (I2,A1,I2,A1,I2)
C***  CHANGE EMBEDDED BLANKS TO ZEROES.
      CALL remblk(id)
 100  CONTINUE
      RETURN
      END
