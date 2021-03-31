      SUBROUTINE getmap(ichan,iloc,irec,ier)
C****************************************************************************
C   PARAMETERS:    ICHAN    IN - CHANNEL NUMBER FOR DT FILE
C                  ILOC     IN - POINTER
C                  IREC     OUT - DT RECORD  ASSOCIATED WITH ILOC
C                  IER      OUT -
C
C***************************************************************************
      DIMENSION irec(16)
      COMMON /io    / nprnt , ioin
      COMMON /dirsy / lastc , lastbk , iwtrn , kdir(256)
C
      ier = 0
      IF ( ichan.NE.lastc ) THEN
         IF ( iwtrn.EQ.1 ) WRITE (lastc,REC=lastbk,IOSTAT=ier,ERR=100)
     &                             kdir
         lastc = ichan
         iwtrn = 0
         lastbk = 0
      ENDIF
      iblk = (iloc-1)/16 + 1
      IF ( iblk.NE.lastbk ) THEN
         IF ( iwtrn.EQ.1 ) WRITE (ichan,REC=lastbk,IOSTAT=ier,ERR=100)
     &                             kdir
         iwtrn = 0
         lastbk = iblk
C
C****    NEW WAY
C
C        OPEN(ICHAN,ACCESS='DIRECT',FORM='UNFORMATTED',RECL=256)
C
         READ (ichan,REC=iblk,IOSTAT=ier,ERR=200) kdir
      ENDIF
      nloc = (iloc-1)*16 + 1
      ioff = nloc - (iblk-1)*256
      DO i = 1 , 16
         irec(i) = kdir(ioff+i-1)
      ENDDO
      GOTO 300
C
 100  CONTINUE
      WRITE (nprnt,99001) ier , lastbk , lastc
99001 FORMAT (' *GETMAP*  ERROR',I5,' WRITING RECORD',I5,' TO CHANNEL ',
     &        I5)
      GOTO 300
 200  CONTINUE
      WRITE (nprnt,99002) ier , iblk , ichan
99002 FORMAT (' *GETMAP*  ERROR',I5,' READING RECORD',I5,
     &        ' FROM CHANNEL ',I5)
C
 300  CONTINUE
      RETURN
      END
