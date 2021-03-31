      SUBROUTINE putfast(index,mbr,record,subject,attrib,extra,ier)
C
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
C             S U B R O U T I N E   P U T F A S T
C
C  FUNCTION:  THIS ROUTINE ADDS A LOGICAL PACKET FOR AN INDIVIDUAL
C             FEATURE INTO THE FAST FILE.
C
C  INPUT PARAMETERS:
C
C             INDEX  = ACTIVE FEATURE POINTER
C             MBR   = 16 ELEMENT ARRAY CONTAINING THE FEATURE MBR
C             RECORD= FEATURE PHYSICAL RECORD POINTER
C             SUBJEC= FEATURE MAJOR ATTRIBUTE POINTER
C             ATTRIB= FEATURE ATTRIBUTE RECORD POINTER
C             EXTRA = RESERVED FOR FUTURE USE
C             IER   = ERROR RETURN (0=NORMAL)
C
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
$Include:'DBCHAN.inc'
$Include:'IO.inc'
$Include:'MFAST.inc'
      INTEGER mbr(8) , record , subject , attrib , extra
C
C  CALCULATE PHYSICAL RECORD LOCATION IN FAST FILE
C
      ier = 0
      IF ( index.EQ.-999 ) THEN
C
C  FORCE WRITE OPTION IF FILE WAS WRITTEN ON
C
         IF ( incore.NE.0 ) WRITE (icdcfa,REC=incore,ERR=300,IOSTAT=ier)
     &                              ifbuff
         GOTO 400
      ELSE
         logrec = ((index-1)/16) + 1
C
         IF ( incore.EQ.0 ) incore = logrec
         IF ( incore.NE.logrec ) THEN
C
C  NEW RECORD.  WRITE OUT OLD RECORD IF IWLST=1
C
            IF ( iwtrn.EQ.1 ) WRITE (icdcfa,REC=incore,ERR=300,IOSTAT=
     &                               ier) ifbuff
            READ (icdcfa,REC=logrec,ERR=100) ifbuff
            incore = logrec
         ENDIF
         GOTO 200
      ENDIF
 100  CONTINUE
      CALL clear(ifbuff,256)
      incore = logrec
C
C
 200  CONTINUE
      iwtrn = 1
      ipoint = (index-1)*16 + 1
      ipoint = ipoint - (logrec-1)*256
      DO i = 1 , 8
C
         ifbuff(i+ipoint-1) = mbr(i)
C
      ENDDO
C
      ifbuff(ipoint+8) = record
C
      ifbuff(ipoint+9) = subject
      GOTO 400
C
 300  CONTINUE
      WRITE (nprnt,99001) ier , incore
99001 FORMAT (' *PUTFAST*  ERROR',I5,' WRITING RECORD',I5)
      ier = -1
C
 400  CONTINUE
      RETURN
      END
