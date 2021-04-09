      SUBROUTINE contin(lines,istop)
      COMMON /clines/ cline
      COMMON /io    / nprnt , ioin
      INTEGER cline
      INTEGER string(80)
      LOGICAL mcomp
C
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
C                  S U B R O U T I N E    C O N T I N
C
C  FUNCTION:  CHECK WITH USER IF SHOULD CONTINUE AFTER N LINES
C
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
      istop = 0
      lines = lines + 1
      IF ( lines-1.GT.21 ) THEN
C
         WRITE (nprnt,99001)
99001    FORMAT (' Continue? ((Y)/N)',\)
         lines = lines + 1
C
         CALL readin(string,80,icardp,iend)
C
         IF ( mcomp(string(1),'N',1,ier) ) istop = 1
         IF ( istop.NE.1 ) THEN
C
            lines = 1
         ENDIF
      ENDIF
C
      RETURN
      END
