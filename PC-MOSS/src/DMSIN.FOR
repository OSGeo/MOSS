C************************************************************************
C
C     NAME:  SUBROUTINE DMSIN
C
C     FUNCTION:  ACCEPTS DEGREES, MINUTES, AND SECONDS FROM TERMINAL
C                INPUT, AND RETURNS THOSE VALUES ALONG WITH THEIR
C                DECIMAL DEGREES EQUIVALENT AS A DOUBLE PRECISION
C                VARIABLE.  THE ROUTINE MAY ALSO BE USED FOR INPUT
C                OF HOURS, MINUTES, AND SECONDS.
C
C     PARAMETERS:  NAME     USE     TYPE     DIMENSION     RANGE
C                  -------------------------------------------------
C                  IIN      I       I*2                    +INTEGER
C                           CHANNEL TO INPUT FROM SCREEN
C                  -------------------------------------------------
C                  IOUT     I       I*2                    +INTEGER
C                           CHANNEL TO OUTPUT TO SCREEN
C                  -------------------------------------------------
C                  DMS      R       R*4          3          REAL
C                           INPUT OF DEGREES, MINUTES, SECONDS
C                  -------------------------------------------------
C                  DPDEG    DP      R*8                    DBL PREC
C                           DECIMAL DEGREES EQUIVALENT OF INPUT
C                  -------------------------------------------------
C                  IC       I       I*2                    +INTEGER
C                           NUMBER OF CHARACTERS INPUT
C                  -------------------------------------------------
C                  IFIRST   I       I*2                    0,1,27,77
C                           INDICATES SPECIAL FIRST CHARACTER
C                           0--NO CHECK FOR SPECIAL CHARACTER
C                           1--CHECK FOR SPECIAL CHARACTER
C                          27--FIRST CHARACTER IS ESCAPE
C                          77--FIRST CHARACTER IS 'M' (MOVE)
C                  -------------------------------------------------
C
C     FILES:  NONE
C
C     CALLING ROUTINE:  CGREF
C
C     ROUTINES REQUIRED:  UFINS--ACCEPTS USER INPUT OF MULTIPLE REALS
C
C     LANGUAGE DEPENDENCE:  FORTRAN
C
C
C**************************************************************************
C
      SUBROUTINE dmsin(iin,iout,dms,dpdeg,ic,ifirst)
C
      INTEGER ifirst , iin , iout , ic
      REAL dms(1)
      DOUBLE PRECISION dpdeg
C
      CALL ufins(iin,iout,dms,3,ic,ifirst)
      IF ( dms(1).GE.0 ) dpdeg = dble(dms(1)) + dble(dms(2))
     &                           /60.D0 + dble(dms(3))/3600.D0
      IF ( dms(1).LT.0 ) dpdeg = dble(dms(1)) - dble(dms(2))
     &                           /60.D0 - dble(dms(3))/3600.D0
      RETURN
      END
