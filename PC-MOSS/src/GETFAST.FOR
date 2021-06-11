      SUBROUTINE getfast(index,mbr,record,subject,attrib,extra,ier)
C******************************************************************************
C
C             S U B R O U T I N E   G E T F A S T
C
C  FUNCTION:  THIS ROUTINE RETRIVES A LOGICAL PACKET FOR AN INDIVIDUAL
C             FEATURE FROM THE FAST FILE.
C
C  INPUT PARAMETERS:
C
C             INDEX = ACTIVE FEATURE POINTER
C             MBR   = 16 ELEMENT ARRAY CONTAINING THE FEATURE MBR
C             RECORD= FEATURE PHYSICAL RECORD POINTER
C             SUBJEC= FEATURE MAJOR ATTRIBUTE POINTER
C             ATTRIB= FEATURE ATTRIBUTE RECORD POINTER
C             EXTRA = RESERVED FOR FUTURE USE
C             IER   = ERROR RETURN (0=NORMAL)
C******************************************************************************
C
$Include:'DBCHAN.inc'
$Include:'IO.inc'
$Include:'MFAST.inc'
      INTEGER mbr(8) , record , subject , attrib , extra
      LOGICAL jopen
C
C.....CALCULATE PHYSICAL RECORD LOCATION IN FAST FILE
C
      ier = 0
      INQUIRE (icdcfa,OPENED=jopen)
      IF ( .NOT.jopen ) THEN
         CALL openf(icdcfa,idesfa,1,512,2,ier)
         IF ( ier.NE.0 ) THEN
            WRITE (*,*) ' Trouble getting item info from fast file ' , 
     &                  ier
            STOP
         ENDIF
      ENDIF
      logrec = ((index-1)/16) + 1
C
      IF ( incore.NE.0 ) THEN
         IF ( incore.EQ.logrec ) GOTO 100
      ENDIF
C
C.....NEW RECORD.  WRITE OUT OLD RECORD IF IWTRN=1
C
      IF ( iwtrn.EQ.1 ) WRITE (icdcfa,REC=incore,IOSTAT=ier,ERR=200)
     &                          ifbuff
      READ (icdcfa,REC=logrec,IOSTAT=ier,ERR=300) ifbuff
      incore = logrec
      iwtrn = 0
C
C.....RECORD DESIRED IS MEMORY RESIDENT
C
 100  CONTINUE
      ipoint = (index-1)*16 + 1
      ipoint = ipoint - (logrec-1)*256
      DO i = 1 , 8
         mbr(i) = ifbuff(i+ipoint-1)
      ENDDO
      record = ifbuff(ipoint+8)
      subject = ifbuff(ipoint+9)
      GOTO 400
 
C.....ERROR HANDELING
C
C
 200  CONTINUE
      WRITE (nprnt,99001) incore , ier
99001 FORMAT (' *GETFAST* ERROR WRITE REC',I5,' TO DESCRIBE.FA ',
     &        ' ERROR ',I5)
      GOTO 400
C
 300  CONTINUE
      WRITE (nprnt,99002) logrec , ier
99002 FORMAT (' *GETFAST* ERROR READING REC',I5,' OF DESCRIBE.FA ',
     &        ' ERROR ',I5)
C
 400  CONTINUE
      RETURN
      END
