      SUBROUTINE read_f77(funit,recnum,buffer,length,ierror)
C-----------------------------------------------------------------------
C
CN  ROUTINE READ-F77-BINARY-RECORD
C
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
C
CP  Purpose:
C
C    This routine uses PRIME primitive I/O to read a FORTRAN 77 style
C  binary record.
CE
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
C
CA  Parameters:
C
C     FUNIT    <Input>  ==  (Integer) Fortran unit number.
C     RECNUM   <Input>  ==  (Integer*4) Record number to read.
C     BUFFER  <Output>  ==  (Array) Buffer read in.
C     LENGTH   <Input>  ==  (Integer) Length of buffer in words.
C     IERROR  <Output>  ==  (Integer) Error code.
CE
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
C
CH  History:
C
C    Abdul Alhazred                03/11/87            Original coding.
CE
C-----------------------------------------------------------------------
      INTEGER funit , length , buffer(length) , ierror
      INTEGER*4 recnum , ipos
C
C
C      IPOS = (RECNUM - 1) * (LENGTH ) + 1
      ipos = recnum
      CALL mx(2,funit,ipos,buffer,length,ierror)
      RETURN
      END
