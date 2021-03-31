      SUBROUTINE write_f7(funit,recnum,buffer,length,ierror)
C-----------------------------------------------------------------------
C
CN  ROUTINE WRITE-F77-BINARY-RECORD
C
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
C
CP  Purpose:
C
C    This routine uses PRIME primitive I/O to write a FORTRAN 77 style
C  binary record.
CE
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
C
CA  Parameters:
C
C     FUNIT    <Input>  ==  (Integer) Fortran unit number.
C     RECNUM   <Input>  ==  (Integer*4) Record number to write.
C     BUFFER   <Input>  ==  (Array) Buffer to write.
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
      INTEGER funit , length , buffer(length) , ierror , from , jpos
      INTEGER fill(1025)/1025*0/
      INTEGER*4 nwords , recnum , ipos , i
C
C
C 10   IPOS = (RECNUM - 1) * (LENGTH ) + 1
 100  CONTINUE
      ipos = recnum
      CALL mx(1,funit,ipos,buffer,length,ierror)
      IF ( ierr.EQ.0 .OR. ierror.EQ.-2 ) RETURN
      IF ( ierror.EQ.-1 ) THEN
         ipos = 0
         CALL mx(3,funit,ipos,jpos,1,ierror)
         from = nwords/(length+1) + 1
         fill(1) = length
         DO i = from , recnum
            ipos = (i-1)*(length) + 1
            CALL mx(1,funit,ipos,fill,length+1,ierror)
         ENDDO
         GOTO 100
      ENDIF
      ipos = ipos + 1
      CALL mx(1,funit,ipos,buffer,length,ierror)
      RETURN
      END
