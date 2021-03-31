      SUBROUTINE putsub(header,nrec,isub,nsub,ichanm,ichan2,ier)
C*****************************************************************************
C
C  FUNCTION :  PUT MOSS PRIMARY SUBJECT INTO SUBJECT TABLE
C
C      PARAMETERS:
C
C      I/O HEADER - MOSS POLYGON HEADER BUFFER
C      O   NREC   - CURRENT RECORD POINTER
C      I   ISUB   - SUBJECT TO ENTER INTO TABLE
C      O   NSUB   - NUMBER OF SORTED SUBJECTS IN 'TEMP2' AFTER FIRST PASS SORT
C      I   ICHANM - DATA CHANNEL TO MOSS MAP
C      I   ICHAN2 - FREE CHANNEL FOR ALL PASSES FOR FILE 'TEMP2'
C          IER    - ERROR INDICATOR (0=NO ERROR)
C
C
C*****************************************************************************
C
      COMMON /io    / nprnt , ioin
C
      INTEGER header(128) , isub(15) , record(16) , buff1(128) , 
     &        buff2(128)
C
      ier = 0
C
C
C......FIND POSITION OF SUBJECT IN FILE 'TEMP2' OR DETERMINE THAT
C......SUBJECT DOES NOT EXIST.
      DO i = 1 , 15
         record(i) = isub(i)
      ENDDO
      CALL fndsub(ichan2,record,nsub,.FALSE.,irec,ier)
C      IF (IREC .LT. 0) ** SUBJECT DOES NOT EXIST
      IF ( irec.LT.0 ) THEN
C
         WRITE (nprnt,99001) isub , irec
99001    FORMAT (' *PUTSUB* SUBJECT NOT FOUND: ',15A2,I7)
      ELSE
C
         IF ( record(16).GT.0 ) THEN
C
C  THIS SUBJECT ALREADY EXISTS
            CALL rdblk(ichanm,irec+2,buff1,1,ier)
            IF ( ier.NE.0 ) GOTO 100
            buff1(17) = buff1(17) + 1
            CALL rdblk(ichanm,record(16),buff2,1,ier)
            IF ( ier.NE.0 ) GOTO 100
            buff2(3) = nrec
            CALL wrblk(ichanm,record(16),buff2,1,ier)
         ELSE
C  THIS SUBJECT IS NEW ...................
            CALL clear(buff1,128)
            DO i = 1 , 15
               buff1(i) = isub(i)
            ENDDO
            buff1(16) = nrec
            buff1(17) = 1
         ENDIF
C
         CALL wrblk(ichanm,irec+2,buff1,1,ier)
C
C......STORE SUBJECT RECORD POINTER
         header(24) = irec + 2
C
C......REPLACE RECORD(16) WITH LAST RECORD # OF THIS FEATURE TYPE
         record(16) = nrec
C
C......NORMAL EXIT
         CALL fndsub(ichan2,record,nsub,.TRUE.,irec,ier)
      ENDIF
      GOTO 200
C
C
C......ERROR PROCESSING
C
 100  CONTINUE
      WRITE (nprnt,99002) ier
99002 FORMAT (' *PUTSUB*  ERROR FROM RDBLK   ERROR ',I5)
C
 200  CONTINUE
      RETURN
      END
