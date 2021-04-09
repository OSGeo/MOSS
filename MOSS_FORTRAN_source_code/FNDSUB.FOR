      SUBROUTINE fndsub(ichan,subj,nsub,insert,irec,ier)
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
C
C                S U B R O U T I N E    F N D S U B
C
C  FUNCTION:  INSERT/FIND SUBJECT/ATTRIBUTE STRING INTO ASCENDING
C             SORT ORDER FILE OF SUBJECTS/ATTRIBUTES
C
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
C
$Include:'IO.inc'
C
      INTEGER subj(16) , low , high , buffer(16) , isub(2) , ibufr(2)
C
      LOGICAL insert , ok
C
      iloc = 0
      irec = 0
      low = 0
      high = nsub + 1
C
C      IF (NSUB .EQ. 0) ** FIRST SUBJECT
      IF ( nsub.EQ.0 ) GOTO 300
C
 100  CONTINUE
      IF ( high-low.LT.2 ) GOTO 300
      irec = (high+low)/2
      IF ( irec.GT.nsub+1 ) GOTO 200
C
      CALL getfsub(ichan,irec,buffer,1,ier)
      IF ( ier.NE.0 ) THEN
         WRITE (nprnt,99001) ichan , nsub , ier , irec
99001    FORMAT ('*FNDSUB* IN: ICHAN,NSUB,IER,REC',4I7)
         GOTO 400
      ELSE
C
         DO i = 1 , 15
            IF ( subj(i).EQ.0 ) subj(i) = 8224
            IF ( buffer(i).EQ.0 ) buffer(i) = 8224
            CALL byteit(subj(i),isub)
            CALL byteit(buffer(i),ibufr)
            DO ill = 1 , 2
c                 IF (SUBJ(I) .LT. BUFFER(I)) GO TO 110
c                 IF (SUBJ(I) .GT. BUFFER(I)) GO TO 120
               IF ( isub(ill).LT.ibufr(ill) ) THEN
C
C......INCOMING SUBJECT LESS THAN STORED
                  high = irec
                  iloc = irec
                  GOTO 100
               ELSEIF ( isub(ill).GT.ibufr(ill) ) THEN
                  GOTO 200
               ENDIF
            ENDDO
         ENDDO
C
C......SUBJECTS ARE EQUAL, DON'T INSERT, DON'T UPDATE NSUB
C      IF (INSERT) REPLACE RECORD IN CASE WORD 16 DIFFERS
         IF ( insert ) THEN
            CALL getfsub(ichan,irec,subj,2,ier)
            IF ( ier.NE.0 ) THEN
C
C......ERROR PROCESSING
               WRITE (nprnt,99002) ichan , nsub , ier , irec
99002          FORMAT ('*FNDSUB* OUT: ICHAN,NSUB,IER,REC',4I7)
               GOTO 400
            ENDIF
         ENDIF
      ENDIF
      IF ( .NOT.insert ) subj(16) = buffer(16)
      GOTO 400
C
C......INCOMING SUBJECT MORE THAN STORED
 200  CONTINUE
      low = irec
      iloc = 0
      GOTO 100
C
C
 300  CONTINUE
      IF ( .NOT.insert ) irec = -1
      IF ( insert ) THEN
C......HAVE TO INSERT THIS SUBJECT IN THE LOW+1 RECORD POSITION
         IF ( iloc.EQ.0 ) iloc = irec + 1
         IF ( iloc.GT.nsub+1 ) iloc = nsub + 1
C
         IF ( iloc.LE.nsub ) THEN
C
            iup = nsub
            index = iup
            DO ii = iloc , iup
               CALL getfsub(ichan,index,buffer,1,ier)
               CALL getfsub(ichan,index+1,buffer,2,ier)
               index = index - 1
            ENDDO
         ENDIF
C
         CALL getfsub(ichan,iloc,subj,2,ier)
         irec = iloc
C
C
C......NORMAL EXIT
         nsub = nsub + 1
      ENDIF
C
C
 400  CONTINUE
      RETURN
      END
