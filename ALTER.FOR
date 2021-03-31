C**********************************************************************
C
C     NAME:  SUBROUTINE ALTER
C
C     FUNCTION:  PERFORMS CATEGORIZE COMMAND WHICH CREATES A DISCRETE MAP
C                FROM A CONTINUOUS MAP BY COMPUTING THE FREQUENCY OF EACH
C                VALUE OCCURRING ON THE CONTINUOUS MAP.
C
C     PARAMETERS:  NAME     USE     TYPE     DIMENSION     RANGE
C                  ------------------------------------------------
C                  IA        I      I*2         NUM        INTEGER
C                  PROFILE OF ELEVATION DATA
C                  ------------------------------------------------
C                  NUM       I      I*2                   +INTEGER
C                  NUMBER OF ELEVATION VALUES PER ROW (OR PROFILE)
C                  ------------------------------------------------
C                  ICONT     I      I*2                   +INTEGER
C                  CONTOUR INTERVAL
C                  ------------------------------------------------
C
C     FILES:  NONE
C
C     ROUTINES REQUIRED:  NONE
C
C     CALLING ROUTINES:  FASTCONT
C
C     SYSTEM DEPENDENCE:  2-BYTE INTEGER
C
C     LANGUAGE DEPENDENCE:  FORTRAN
C
C****************************************************************************
C
      SUBROUTINE alter(ia,num,icont)
CC
      DIMENSION ia(1)
CC
CC MODIFY THE GRID ELEVATION WHEN THAT ELEVATION VALUE IS AN EVEN
CC MULTIPLE OF THE CONTOUR INTERVAL.
CC
      iepsln = 1
      ll = num - 1
      ihgt1 = ia(1)
      IF ( icont.EQ.0 ) RETURN
      IF ( iabs(mod(ihgt1,icont)).EQ.0 ) THEN
         ihgt2 = ia(2)
c
         ia(1) = isign(iepsln,ihgt2-ihgt1) + ihgt1
      ENDIF
      IF ( num.NE.2 ) THEN
c
         DO i = 2 , ll
            ihgt1 = ia(i)
            IF ( iabs(mod(ihgt1,icont)).EQ.0 ) THEN
               ihgt2 = ia(i+1)
               ihgt0 = ia(i-1)
               ia(i) = isign(iepsln,(ihgt2+ihgt0)/2-ihgt1) + ihgt1
            ENDIF
c
         ENDDO
      ENDIF
c
      IF ( iabs(mod(ihgt1,icont)).EQ.0 ) THEN
         ihgt0 = ia(num-1)
         ia(num) = isign(iepsln,ihgt0-ihgt1) + ihgt1
      ENDIF
c
      RETURN
      END
