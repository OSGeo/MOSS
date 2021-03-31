      SUBROUTINE byteit(ibuff,iout)
C
      DIMENSION iout(2)
C
      ihold = ishft(ibuff,8)
      irhgt = ishft(ihold,-8)
      ileft = ishft(ibuff,-8)
      iout(1) = irhgt
      iout(2) = ileft
      RETURN
      END
