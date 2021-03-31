      SUBROUTINE getat1(ichan,ityp,mwords,locate,ibuff,item,isubj,value,
     &                  ier)
      COMMON /io    / nprnt , ioin
      DIMENSION ibuff(128) , isubj(72) , iv(2)
      CHARACTER*10 numb
      INTEGER record
      EQUIVALENCE (rnum,iv(1))
      DATA iblk/'  '/
C**********************************************************************
C
C  GET A SINGLE VALUE FOR A SINGLE ATTRIBUTE GIVEN A
C  POLYGON ITEM NUMBER
C
C  PROGRAMMED BY BILL TURNER, AUTOMETRIC, INC. JAN 86
C  TAKEN FROM GETAT.FR  THE MAIN DIFFERENCE IS THAT GETAT1 PASSES ISUBJ
C  BACK WITH THE ENTIRE LENGTH OF A CHARACTER ATTRIBUTE, NOT JUST THE
C  FIRST 15 CHARACTERS.  LATER, IT MAY BE A GOOD IDEA TO PHASE THE
C  ORIGINAL OLD GETAT OUT.
C
C**********************************************************************
      value = 0.0
      DO i = 1 , 72
         isubj(i) = 0
      ENDDO
C
C  CALCULATE RECORD IN WHICH VALUE LIES
C
      nwords = iabs(mwords)
      limit = 128/nwords
      record = ((item-1)/limit) + 1
C
C  READ CORRECT BLOCK
C
      CALL rdblk(ichan,record+locate-1,ibuff,1,ier)
      IF ( ier.EQ.0 ) THEN
C
C  CALCULATE LOCATION IN BLOCK FOR ATTRIBUTE
C
         base = aint((record-1)*float(limit))
         index = (((item-base-1)*nwords)+1)
C
         IF ( ityp.EQ.1 ) THEN
            value = ibuff(index)
C
            kl = 0
            DO i = 1 , 10
               IF ( numb(i:i).NE.' ' ) THEN
                  kl = kl + 1
                  isubj(kl) = ichar(numb(i:i))
                  IF ( isubj(kl).LT.48 .OR. isubj(kl).GT.57 ) isubj(kl)
     &                  = 32
               ENDIF
            ENDDO
         ELSEIF ( ityp.EQ.2 ) THEN
C
C  REAL
C
            iv(1) = ibuff(index)
            iv(2) = ibuff(index+1)
            value = rnum
            WRITE (numb,99001) rnum
99001       FORMAT (F10.3)
C
            kl = 0
            DO i = 1 , 10
               IF ( numb(i:i).NE.' ' ) THEN
                  kl = kl + 1
                  isubj(kl) = ichar(numb(i:i))
               ENDIF
            ENDDO
         ELSE
C
C  CHARACTER
C
            DO i = 1 , nwords
               isubj(i) = ibuff(index+i-1)
               IF ( ibuff(index+i-1).NE.0 .AND. 
     &              (ibuff(index+i-1).LT.32 .OR. ibuff(index+i-1)
     &              .GT.126) ) THEN
                  isubj(i) = 32
               ENDIF
            ENDDO
C        CALL PACKC(JCHAR,ISUBJ,72)
C        CALL PAD(ISUBJ,36)
            DO l = nwords + 1 , 72
               isubj(l) = 0
            ENDDO
         ENDIF
      ENDIF
C
C  ERROR PROCESSING
C
C
      RETURN
C
C  INTEGER
C
C        write(NUMB,1101) IBUFF(INDEX)
99002 FORMAT (I10)
      END
