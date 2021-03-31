C
C ***************************************************************
C
C
C               SUBROUTINE R T E M P
C
C  FUNCTION:  HANDLES TEMP FILE FOR REPORT PROGRAM
C
C  PROGRAMMED BY MIKEF FIEBACH TGS  BLM/DSC  SEPT 1985
C
C     RECIN  -  INPUT RECORD NUMBER
C     IRECOUT -  OUTPUT RECORD NUMBER, BETWEEN 1 AND 320000
C     IFILE   -  POINTER TO SCRATCHFILE FOR RECORD RECIN
C     IFILEL  -  PREBIOUS POINTER TO SCRATCHFILE
C     CRE     -  IF TRUE, THEN CREATE THE TEMP FILE
C
C **************************************************************
C
      SUBROUTINE rtemp(recin,irecout,ifile,ifilel,cre)
      COMMON /io    / nprnt , ioin
      LOGICAL cre
      INTEGER tfina(4,10) , fina(25)
      DATA tfina/'TE' , 'MP' , '01' , '  ' , 'TE' , 'MP' , '02' , '  ' , 
     &     'TE' , 'MP' , '03' , '  ' , 'TE' , 'MP' , '04' , '  ' , 
     &     'TE' , 'MP' , '05' , '  ' , 'TE' , 'MP' , '06' , '  ' , 
     &     'TE' , 'MP' , '07' , '  ' , 'TE' , 'MP' , '08' , '  ' , 
     &     'TE' , 'MP' , '09' , '  ' , 'TE' , 'MP' , '10' , '  '/
C
C......IF RECORD POINTER IS LARGER THAN 32K, CALCULATE APROPRIATE
C      POINTER INTO 1 OF 10 32 K DIRECT ACCESS FILES
C
      ifile = aint(recin/32000.+1.)
      irecout = amod(recin,32000.) + 1
      IF ( ifilel.NE.ifile ) THEN
         CLOSE (88,IOSTAT=ier)
         IF ( ier.NE.0 ) WRITE (nprnt,99001) ier
99001    FORMAT ('CLOSE ERROR ',I5,' FROM RTEMP')
         DO l = 1 , 25
            fina(l) = 32
         ENDDO
         DO i = 1 , 4
            fina(i) = tfina(i,ifile)
         ENDDO
         icre = 1
         IF ( cre ) THEN
            icre = 0
         ENDIF
         CALL openf(88,fina,icre,2,2,ier)
         IF ( ier.NE.0 ) WRITE (nprnt,99002)
99002    FORMAT ('OPENF ERROR ',I5,' FROM RTEMP')
         ifilel = ifile
      ENDIF
      IF ( ifile.GT.10 ) WRITE (nprnt,99003)
99003 FORMAT ('FILE TOO LARGE FOR REPORT ')
      RETURN
      END
