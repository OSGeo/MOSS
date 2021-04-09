      SUBROUTINE prhead(rptname,lines,npages)
C******************************************************************************
C    Print a header for reports
C
C******************************************************************************
$Include:'IO.inc'
      CHARACTER*20 rptname
C
      lines = lines + 4
C
      WRITE (nprnt,99001) rptname , npages
99001 FORMAT (/,2X,A20,21X,'PAGE',I4,/)
C
C
      RETURN
      END
