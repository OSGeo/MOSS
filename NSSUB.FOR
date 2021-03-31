      SUBROUTINE nssub(ichan,subj,nsub,insert,irec,ier)
C
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
C
C                S U B R O U T I N E    N S S U B
C
C  FUNCTION:  INSERT/FIND SUBJECT/ATTRIBUTE STRING INTO NON-SORTED
C              FILE OF SUBJECTS/ATTRIBUTES
C
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
C
      COMMON /io    / nprnt , ioin
C
      INTEGER subj(16) , low , high , buffer(16)
C
      LOGICAL insert , ok
C
      iloc1 = 0
      iloc2 = 0
      low = 0
      irec = 0
      high = nsub + 1
C
C      IF (NSUB .EQ. 0) ** FIRST SUBJECT
      IF ( nsub.EQ.0 .OR. insert ) THEN
C
C
         IF ( .NOT.insert ) irec = -1
         IF ( insert ) THEN
            nsub = nsub + 1
C
C......NORMAL EXIT
            CALL getfsub(ichan,nsub,subj,2,ier)
C
C......DEBUG FORMATS
99001       FORMAT (1X,15A2,T35,'LOC',I7,'FOUND AT REC',I7)
99002       FORMAT (1X,15A2,T35,'LOC',I7,'INSERTED AT REC',I7)
99003       FORMAT (' FNDSUB:EXIT: NSUB,INSERT,IREC',I7,L4,I7)
         ENDIF
         GOTO 200
      ELSE
C
         DO irec = 1 , nsub
            CALL getfsub(ichan,irec,buffer,1,ier)
            IF ( ier.NE.0 ) GOTO 100
            DO ll = 1 , 15
               IF ( subj(ll).NE.buffer(ll) ) GOTO 50
            ENDDO
            GOTO 200
 50      ENDDO
      ENDIF
C
C......ERROR PROCESSING
 100  CONTINUE
      WRITE (nprnt,99004) ichan , nsub , ier , irec
99004 FORMAT ('*FNDSUB* IN: ICHAN,NSUB,IER,REC',4I7)
C
 200  CONTINUE
      RETURN
      END
