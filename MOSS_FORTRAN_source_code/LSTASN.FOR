      SUBROUTINE lstasn(lines,itype,ichan,ier)
C
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
C                 S U B R O U T I N E    L S T A S N
C
C
C  FUNCTION:  PRINT OUT MAP FONT ASSIGNMENTS
C
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
$Include:'IO.inc'
      COMMON /fls   / iunit , ifile(5)
      COMMON /work  / ibuff(128) , map(25) , icom(10) , jcom(10)
C
      ier = 0
C
      lines = 0
C
C  CALCULATE ADDRESS OF FIRST RECORD
C
      CALL rdblk(ichan,1,ibuff,1,ier)
C
C  HAS THE MAP BEEN FONT ASSIGNED ?
C
      IF ( ibuff(120).LE.0 .OR. ibuff(120).GT.128 ) THEN
C
C  ERRORS
C
         WRITE (nprnt,99001)
99001    FORMAT (' This map has not been cartographically assigned.')
         lines = lines + 2
         RETURN
      ELSE
C
         IF ( itype.NE.2 ) nrec = ibuff(64) + 2
         IF ( itype.EQ.2 ) nrec = 3
C
C  GET NUMBER OF SUBJECTS
C
         nsubs = ibuff(67)
         WRITE (nprnt,99002)
99002    FORMAT ('        SUBJECT                 FONT #        COLOR ')
C
C       CALL ANMODE
         DO i = 1 , nsubs
            CALL rdblk(ichan,nrec,ibuff,1,ier)
            nrec = nrec + 1
            WRITE (nprnt,99003) (ibuff(j),j=1,15) , ibuff(20) , 
     &                          ibuff(26)
            lines = lines + 1
99003       FORMAT (1X,15A2,4X,I4,4X,I4)
C
         ENDDO
C
         RETURN
      ENDIF
C
      END
