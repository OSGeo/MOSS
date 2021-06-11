      SUBROUTINE mdyhms(mon,iday,iyr,ihr,min,isec,i100)
C       Gets date and time from system routines
      INTEGER*2 mon , iday , iyr , ihr , min , isec , i100
      INTEGER*2 idate(3) , itime(3)
      CALL getdat(iyr,mon,iday)
      CALL gettim(ihr,min,isec,i100)
C
      RETURN
      END
