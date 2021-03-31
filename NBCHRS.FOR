C************************************************************************
C
C  Name: NBCHRS
C
C  Purpose: TO COUNT THE # OF NON-BLANK CHARS IN THE LABEL 'ILABEL'
C
C  Parameters:
C       Name    Use     Type    Dimension   Description
C       ------  ------  ------  ---------   ---------------------------
C       ILABEL   IN    INTEGER      1       packed character string
C       NCHAR    IN    INTEGER      1       number of chars in string
C       IFRST    OUT   INTEGER      1       number of first char in string
C       ILAST    OUT   INTEGER      1       number of last char in string
C
C  Files:
C       Unit    File name or description
C       ------  -------------------------------------------------------
C            NONE
C
C  Calls: GTB
C  Notes:
C******************************************************************************
C
      SUBROUTINE nbchrs(ilabel,nchar,ifrst,ilast,ier)
C
      COMMON /io    / nprnt , ioin
C
      INTEGER ilabel(1)
C
      ier = 0
      ifrst = 0
      ilast = 0
C
C     PARSE UNTIL FIRST NON-BLANK CHARACTER
C
      DO i = 1 , nchar
         CALL goff(iget,i,ilabel(1))
         IF ( iget.NE.32 .AND. iget.NE.0 ) GOTO 100
      ENDDO
C
C     PARSE UNTIL END OF STRING
C
 100  CONTINUE
      ifrst = i
      DO i = ifrst , nchar
         CALL goff(iget,i,ilabel(1))
         IF ( iget.EQ.32 .OR. iget.EQ.0 ) GOTO 200
      ENDDO
C
 200  CONTINUE
      ilast = i - 1
C
      RETURN
      END
