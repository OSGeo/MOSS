      SUBROUTINE header(record)
C************************************************************************
CC
CC NAME : SUBROUTINE HEADER
CC
CC FUNCTION : TO OUTPUT THE ATTRIBUTE INFORMATION IN A NEAT TABLE FORM
CC
CC PARAMETERS : RECORD : AN ARRAY DEMENSIONED 128. IT CONTAINS THE ATTRIBUTE
CC                       INFORMATION THAT IS TO BE LISTED OUT.
CC
C************************************************************************
CC
$Include:'IO.inc'
CC
      COMMON /unit  / chanel , atrbte , polygon , adress
CC
      INTEGER chanel , atrbte , polygon , adress , action , record(128)
CC
      WRITE (nprnt,99001)
99001 FORMAT (' ','ATTRIBUTE LIST')
      WRITE (nprnt,99002)
99002 FORMAT (' ',72('-'))
      WRITE (nprnt,99003) record(1)
99003 FORMAT (' ','ID = ',I7)
      WRITE (nprnt,99002)
      WRITE (nprnt,99004) (record(i),i=2,11)
99004 FORMAT (' ','KEY = ',10A1)
      WRITE (nprnt,99002)
      WRITE (nprnt,99005)
99005 FORMAT (' ','THE DESCRIPTOR = ')
      WRITE (nprnt,99006) (record(i),i=12,83)
99006 FORMAT (' ',72A1)
      WRITE (nprnt,99002)
      IF ( record(84).EQ.1 ) WRITE (nprnt,99007)
      IF ( record(84).EQ.2 ) WRITE (nprnt,99008)
      IF ( record(84).EQ.3 ) WRITE (nprnt,99009)
      WRITE (nprnt,99002)
99007 FORMAT ('  THE DATA TYPE IS INTEGER ')
99008 FORMAT ('  THE DATA TYPE IS REAL    ')
99009 FORMAT ('  THE DATA TYPE IS CHARACTER STRING ')
CC
      RETURN
      END
