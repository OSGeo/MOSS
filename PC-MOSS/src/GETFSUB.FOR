      SUBROUTINE getfsub(ichan,index,record,ido,ier)
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
C             S U B R O U T I N E   G E T F S U B
C
C  FUNCTION:  THIS ROUTINE RETRIVES A LOGICAL PACKET FOR AN INDIVIDUAL
C             FEATURE INTO THE FAST FILE.
C
C  INPUT PARAMETERS:
C
C             ICHAN =  CHANNEL TEMP FILE IS OPENNED ON
C             INDEX  = ACTIVE FEATURE POINTER
C             RECORD= FEATURE PHYSICAL RECORD POINTER
C             IDO   =  IF NOT EQUAL 2  UPDATE RECORD
C             IER   = ERROR RETURN (0=NORMAL)
C
C
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
$Include:'IO.inc'
      COMMON /tempsort/ tempbuf(128) , iptr , iwrite , lastrec
      INTEGER tempbuf , record(16)
C
C  CALCULATE PHYSICAL RECORD LOCATION IN FAST FILE
C
      ier = 0
      logrec = ((index-1)/8) + 1
C
      IF ( iptr.EQ.0 ) CALL clear(tempbuf,128)
      IF ( iptr.NE.0 ) THEN
         IF ( iptr.EQ.logrec ) GOTO 100
      ENDIF
C
C  NEW RECORD.  WRITE OUT OLD RECORD IF IWLST=1
C
      IF ( iwrite.EQ.1 ) THEN
         CALL wrblk(ichan,iptr,tempbuf,1,ier)
         IF ( ier.NE.0 ) GOTO 200
      ENDIF
      IF ( logrec.LE.lastrec ) THEN
         CALL rdblk(ichan,logrec,tempbuf,1,ier)
         IF ( ier.NE.0 ) GOTO 200
      ENDIF
      lastrec = max0(lastrec,logrec)
      iptr = logrec
      iwrite = 0
C
C
 100  CONTINUE
      ipoint = (index-1)*16 + 1
      ipoint = ipoint - (logrec-1)*128
C
      IF ( ido.NE.2 ) THEN
         DO i = 1 , 16
            record(i) = tempbuf(i+ipoint-1)
         ENDDO
         GOTO 200
      ENDIF
C
      iwrite = 1
C
      DO i = 1 , 16
         tempbuf(i+ipoint-1) = record(i)
      ENDDO
C
C
 200  CONTINUE
      RETURN
      END
