      SUBROUTINE putmap(ichan,iloc,name,ier)
C ******************************************************************
C    FUNCTION :  INSERT NAME INTO .DT
C
      DIMENSION name(16)
      COMMON /io    / nprnt , ioin
      COMMON /dirsy / lastc , lastbk , iwtrn , kdir(256)
C
      ier = 0
C
      IF ( ichan.NE.lastc ) THEN
         IF ( iwtrn.EQ.1 ) WRITE (lastc,REC=lastbk,IOSTAT=ier,ERR=200)
     &                             kdir
         lastc = ichan
         iwtrn = 0
         lastbk = 0
      ENDIF
C
      iblk = (iloc-1)/16 + 1
      IF ( iblk.NE.lastbk ) THEN
         IF ( iwtrn.EQ.1 ) WRITE (ichan,REC=lastbk,IOSTAT=ier,ERR=200)
     &                             kdir
C
         lastbk = iblk
         READ (ichan,REC=iblk,IOSTAT=ier,ERR=300) kdir
      ENDIF
C
 100  CONTINUE
      nloc = (iloc-1)*16 + 1
      ioff = nloc - (iblk-1)*256
      iwtrn = 1
      DO i = 1 , 16
         kdir(ioff+i-1) = name(i)
      ENDDO
      GOTO 400
C
 200  CONTINUE
      WRITE (nprnt,99001) ier , lastbk , lastc
99001 FORMAT (' *PUTMAP*  ERROR',I5,' WRITING RECORD',I5,' TO CHANNEL ',
     &        I5)
      GOTO 400
C
C......END OF FILE FROM READ THEN NULL FILL BLOCK AND CONTINUE
 300  CONTINUE
      IF ( ier.EQ.-1 ) THEN
C
C......NULL FILL BLOCK WHEN EOF ON READ
         DO i = 1 , 256
            kdir(i) = 0
         ENDDO
         GOTO 100
      ELSE
         WRITE (nprnt,99002) ier , iblk , ichan
99002    FORMAT (' *PUTMAP*  ERROR',I5,' READING RECORD',I5,
     &           ' FROM CHANNEL ',I5)
      ENDIF
C
 400  CONTINUE
      RETURN
      END
