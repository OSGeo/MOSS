      SUBROUTINE gtmap(icom,idd)
C
C  RETURN MAPNAME FROM STRUCTURE TABEL
C ************************************************************************
C  FUNCTION:   CHECK FOR VALID IDD
C
C     PARAMETERS:  ICOM  - OUT - ACTIVE TABLE MAP NAME ASSOCIATED WITH IDD
C                  IDD   - IN  - ACTIVE TABLE INDEX INTO ISTRCT
C
C*************************************************************************
C
$Include:'WHMAP.inc'
C
      DIMENSION icom(10)
C
      IF ( idd.GT.nstrct ) RETURN
C
C  GET MAP NAME
C
      kdd = idd
      DO i = 1 , 5
         icom(i) = 0
         icom(i+5) = 0
         icom(i) = istrct(kdd+2)
         kdd = kdd + 1
      ENDDO
C
      RETURN
      END
