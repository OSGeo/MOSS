C***********************************************************************
C
C     NAME:  SUBROUTINE DESHDR
C
C     FUNCTION:  DISPLAYS THE PROJECTION INFORMATION FOR A MAP.
C
C     PARAMETERS:  NAME     USE     TYPE     DIMENSION     RANGE
C                  -------------------------------------------------
C                  IHEAD     I      I*2         256      +INTEGER
C                  HEADER INFORMATION
C                  -------------------------------------------------
C                  MAPNAM    I      I*2         10         CHAR
C                  MAP NAME
C                  -------------------------------------------------
C                  LINES     I/O    I*2                    +INTEGER
C                  NUMBER OF LINES OF OUTPUT
C                  -------------------------------------------------
C                  IER       O      I*2                    INTEGER
C                  ERROR FLAG
C                  -------------------------------------------------
C
C     FILES:  OUTPUT FILE
C
C     ROUTINES REQUIRED:  PROVW
C
C     CALLING ROUTINE:  DESC  LABHDR
C
C     SYSTEM DEPENDENCE:  2-BYTE INTEGER
C
C     LANGUAGE DEPENDENCE:  FORTRAN
C
C***********************************************************************
C
      SUBROUTINE desprj(ihead,mapnam,lines,ier)
      COMMON /io    / nprnt , ioin
C
      INTEGER ihead(256) , mapnam(25) , lines , ier , ivalu(64)
      DOUBLE PRECISION rvalu(15)
      EQUIVALENCE (ivalu(5),rvalu(1))
C
C......MOVE TO STACK ARRAY TO ALLOW EQUIVALENCING .....
      DO i = 1 , 64
         ivalu(i) = ihead(192+i)
      ENDDO
C
C......PRINT PROJECTION INFORMATION UNLESS NONE HAS BEEN ASSIGNED ......
C
      WRITE (nprnt,99001) (mapnam(i),i=1,12)
99001 FORMAT (/,' MAP ',12A2)
      lines = lines + 2
      IF ( ivalu(1).NE.-1 ) THEN
C
C......PRINT OUT THE PROJECTION INFORMATION .....
         CALL provw(nprnt,ivalu(1),ivalu(2),ivalu(3),ivalu(4),rvalu(1))
         lines = lines + 14
      ELSE
C
C......NONE HAS BEEN ASSIGNED ......
         WRITE (nprnt,99002) (mapnam(i),i=1,25)
99002    FORMAT (' *WARNING*  A PROJECTION HAS NOT BEEN ASSIGNED TO ',
     &           25A2)
         lines = lines + 2
      ENDIF
C
C......NORMAL RETURN ......
C
      RETURN
      END
