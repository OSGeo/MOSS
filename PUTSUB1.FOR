      SUBROUTINE putsub1(iarr,nrec,isub,khash,itsub,maxsub,ichan,ier)
      COMMON /io    / nprnt , ioin
      DIMENSION ibuff(128)
      DIMENSION icard(80)
      COMMON /pssss / nnsub
      DIMENSION jbuff(128)
      DIMENSION iarr(128) , isub(1) , khash(maxsub) , itsub(maxsub)
C
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
C              S U B R O U T I N E    P U T S U B
C
C  FUNCTION:  PUT MOSS PRIMARY SUBJECT INTO SUBJECT TABLE
C             LARGELY REPLACED BY PUTSUB WHICH DOES A BINARY SORT AND SEARCH
C
C  PROGRAMMED BY CARL REED, AUTOMETRIC, INC. AUG. 1982
C
C  INPUT PARAMETERS:
C
C       IARR  =  MOSS POLYGON HEADER BUFFER
C       NREC    =  CURRENT RECORD POINTER
C       ISUB  =  SUBJECT TO ENTER INTO TABLE
C       KHASH = WORKSPACE FOR HASH KEYS
C       ITSUB =  WORKSPACE FOR SUBJECT POINTERS
C       MAXSUB=  MAX NUMBER OF ALLOWED SUBJECTS
C       ICHAN  =  DATA CHANNEL TO MOSS MAP
C
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
      ier = 0
C
      CALL clear(icard,80)
      DO i = 1 , 15
         icard(i) = isub(i)
      ENDDO
      CALL unpack(icard,80)
      CALL chash(icard,30,32003,ihash,1)
C
      kk = 0
 100  CONTINUE
      kk = kk + 1
      IF ( kk.GT.nnsub ) THEN
C
         nnsub = nnsub + 1
         IF ( nnsub.GT.maxsub ) THEN
C
            ier = 0
            WRITE (nprnt,99001) maxsub
99001       FORMAT (' *PUTSUB1*  # OF SUBJECTS GREATER THAN',I5,
     &              '  ERROR',I5)
            RETURN
         ELSE
            itsub(nnsub) = nrec
            iarr(3) = 0
            khash(nnsub) = ihash
            CALL clear(jbuff,128)
            DO i = 1 , 15
               jbuff(i) = isub(i)
            ENDDO
            jbuff(16) = nrec
            jbuff(17) = 1
         ENDIF
      ELSE
         IF ( ihash.NE.khash(kk) ) GOTO 100
         CALL rdblk(ichan,kk+2,jbuff,1,ier)
C
         DO i = 1 , 15
            IF ( jbuff(i).NE.isub(i) ) GOTO 100
         ENDDO
C
         jbuff(17) = jbuff(17) + 1
         match = itsub(kk)
         CALL rdblk(ichan,match,ibuff,1,ier)
         ibuff(3) = nrec
         CALL wrblk(ichan,match,ibuff,1,ier)
         itsub(kk) = nrec
      ENDIF
      CALL wrblk(ichan,kk+2,jbuff,1,ier)
C
C  STORE SUBJECT RECORD POINTER
C
      iarr(24) = kk + 2
      RETURN
      END
