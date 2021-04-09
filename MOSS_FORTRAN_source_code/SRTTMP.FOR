      SUBROUTINE srttmp(ichan1,ichan2,insub,numsub,ier)
C****************************************************************************
C   SORT TEMPORARY FILE OF SUBJECTS
C
C   PARAMETERS:   ICHAN1 - IN -  Channel of in file
C                 ICHAN2 - IN -  Channel of out file
C                 INSUB  - IN -  number of subjects in file to be sorted
C                 NUMSUB - OUT - number of subjects written out
C
C****************************************************************************
$Include:'IO.inc'
C
      INTEGER buff(128)
      INTEGER subject(16)
C
C
      ier = 0
      numsub = 0
C
C......GET SUBJECT, NVERT, ISLAND COUNT INFO, UPDATE SUBJECT #
      DO index = 1 , insub
         CALL rdblk(ichan1,index,buff,1,ier)
         IF ( ier.NE.0 ) THEN
C
            WRITE (nprnt,99001) insub , ier
99001       FORMAT ('*SRTTMP* RDBLK  RECORD ',I5,'ERROR   ',I5)
            GOTO 100
         ELSE
C
C.........PUT SUBJECT IN SORTED FILE IF UNIQUE
            subject(16) = 0
            DO ii = 1 , 15
               subject(ii) = buff(ii)
            ENDDO
            CALL fndsub(ichan2,subject,numsub,.TRUE.,irec,ier)
            IF ( ier.NE.0 ) THEN
C
               WRITE (nprnt,99002) (subject(i),i=1,15) , numub
99002          FORMAT ('*SRTTMP* FNDSUB: SUBJECT,NUMSUB',/,15A2,I5)
               GOTO 100
            ENDIF
         ENDIF
C
C
C
C......NORMAL EXIT
      ENDDO
C
 100  CONTINUE
      RETURN
      END
