C***********************************************************************
C
C     NAME:  SUBROUTINE DESHDR
C
C     FUNCTION:  DISPLAYS THE HEADER INFORMATION FOR A MAP.
C
C     PARAMETERS:  NAME     USE     TYPE     DIMENSION     RANGE
C                  -------------------------------------------------
C                  IHEAD     I      I*2         256        +INTEGER
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
C     ROUTINES REQUIRED:  NONE
C
C     CALLING ROUTINE:  DESC  LABHDR
C
C     SYSTEM DEPENDENCE:  2-BYTE INTEGER
C
C     LANGUAGE DEPENDENCE:  FORTRAN
C
C***********************************************************************
C
      SUBROUTINE deshdr(ihead,mapnam,lines,ier)
      COMMON /io    / nprnt , ioin
C
      INTEGER ihead(256) , mapnam(25) , lines , ier , itydes(6,10) , 
     &        ivalu(24)
      REAL rvalu(12)
      LOGICAL cell
      EQUIVALENCE (ivalu(1),rvalu(1))
C
      DATA itydes/'PO' , 'IN' , 'T ' , '  ' , '  ' , '  ' , 'LI' , 
     &     'NE' , '  ' , '  ' , '  ' , '  ' , 'PO' , 'LY' , 'GO' , 
     &     'N ' , '  ' , '  ' , '  ' , '  ' , '  ' , '  ' , '  ' , 
     &     '  ' , 'EL' , ' P' , 'OI' , 'NT' , '  ' , '  ' , 'DI' , 
     &     'CH' , 'OT' , 'OM' , 'OU' , 'S ' , 'DI' , 'SC' , 'RE' , 
     &     'TE' , '  ' , '  ' , 'CO' , 'NT' , 'IN' , 'UO' , 'US' , 
     &     '  ' , 'MU' , 'TI' , 'VA' , 'LU' , 'E ' , '  ' , 'TE' , 
     &     'XT' , '  ' , '  ' , '  ' , '  '/
C
C......MOVE TO STACK ARRAY TO ALLOW EQUIVALENCING .....
C
      DO i = 1 , 24
         ivalu(i) = ihead(i+67)
      ENDDO
C
C......DETERMINE IF CELL DATA ......
      cell = .FALSE.
      IF ( ihead(62).GE.6 .AND. ihead(62).LE.9 ) cell = .TRUE.
      ityp = ihead(62)
      IF ( ityp.GE.11 .AND. ityp.LE.13 ) ityp = ityp - 10
C
C......PRINT HEADER INFORMATION ......
C
      WRITE (nprnt,99001) (mapnam(i),i=1,12)
99001 FORMAT (/,' MAP ',12A2)
      WRITE (nprnt,99002) (ihead(i),i=20,49)
99002 FORMAT (' DESCRIPTION   ',30A2)
      WRITE (nprnt,99003) (ihead(i),i=16,19) , (ihead(i),i=53,57)
99003 FORMAT (' STUDY AREA    ',4A2,'  PROJECTION ',5A2)
      WRITE (nprnt,99004) (ihead(i),i=12,15) , (ihead(i),i=2,11) , 
     &                    (ihead(i),i=51,52)
99004 FORMAT (' DATE          ',4A2,'  SOURCE ',10A2,' VINTAGE ',2A2)
      WRITE (nprnt,99005) ihead(62) , (itydes(i,ityp),i=1,6) , ihead(67)
99005 FORMAT (' TYPE  ',I3,1X,6A2,'  SUBJECTS   ',I5)
      lines = lines + 7
C
      i = ihead(77)
      IF ( i.EQ.0 .OR. i.EQ.32 ) i = 77
      IF ( .NOT.cell ) WRITE (nprnt,99006) ihead(63) , i
99006 FORMAT (' ITEMS ',I5,13X,'COORDINATE CONVERSION  ',A2)
      IF ( cell ) THEN
C
         WRITE (nprnt,99007) rvalu(7) , rvalu(8)
99007    FORMAT (' CELL HEIGHT',F11.4,'  CELL WIDTH ',F11.4)
         WRITE (nprnt,99008) ihead(60) , ihead(61)
99008    FORMAT (' NUMBER OF ROWS',I8,'  NUMBER OF COLUMNS',I5)
         lines = lines + 2
         IF ( mapnam(10).NE.6 ) WRITE (nprnt,99009) rvalu(6) , 
     &                                 (rvalu(6)*4046.856422)/10000.
99009    FORMAT (' CELL ACRES',F12.4,' CELL HECTARES',F12.4)
         IF ( ihead(62).EQ.8 ) WRITE (nprnt,99010) rvalu(11) , rvalu(12)
99010    FORMAT (' MINIMUM VALUE ',F13.4,'      MAXIMUM VALUE ',F13.4)
         IF ( ihead(62).EQ.6 ) WRITE (nprnt,99011) rvalu(6) , rvalu(9)
99011    FORMAT (' CELL ACRES',F12.4,'  CELLS SET',F13.1)
         lines = lines + 2
      ENDIF
C
      WRITE (nprnt,99012) rvalu(3) , rvalu(4) , rvalu(2) , rvalu(1)
99012 FORMAT (' MBR: SOUTH',F13.4,' NORTH',F13.4,' EAST',F13.4,' WEST',
     &        F13.4)
      lines = lines + 2
C
C......NORMAL RETURN ......
C
      RETURN
      END
