      SUBROUTINE getat(ichan,ityp,mwords,locate,ibuff,item,map,value,
     &                 ier)
      COMMON /io    / nprnt , ioin
      DIMENSION ibuff(128) , map(25) , jchar(30) , iv(2)
      INTEGER record
      CHARACTER numb*10
      EQUIVALENCE (rnum,iv(1))
      DATA iblk/'  '/
C
C  GET A SINGLE VALUE FOR A SINGLE ATTRIBUTE GIVEN A
C  POLYGON ITEM NUMBER
C
C
      value = 0.0
      DO i = 1 , 25
         map(i) = iblk
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
C
C  INTEGER
C
            WRITE (numb,99001) ibuff(index)
99001       FORMAT (I10)
            value = ibuff(index)
C
            loop = 10
            kl = 0
            DO i = 1 , 10
               IF ( numb(i:i).NE.' ' ) THEN
                  kl = kl + 1
                  jchar(kl) = ichar(numb(i:i))
               ENDIF
            ENDDO
         ELSEIF ( ityp.EQ.2 ) THEN
C
C  REAL
C
            iv(1) = ibuff(index)
            iv(2) = ibuff(index+1)
            value = rnum
            WRITE (numb,99002) rnum
99002       FORMAT (F10.3)
C
            loop = 10
            kl = 0
            DO i = 1 , 10
               IF ( numb(i:i).NE.' ' ) THEN
                  kl = kl + 1
                  jchar(kl) = ichar(numb(i:i))
               ENDIF
 
            ENDDO
         ELSE
C
C  CHARACTER
C
            loop = nwords
            IF ( loop.GT.30 ) loop = 30
            DO l = 1 , 30
               jchar(l) = 32
            ENDDO
            DO i = 1 , loop
               jchar(i) = ibuff(index+i-1)
            ENDDO
         ENDIF
      ENDIF
C
      CALL packc(jchar,map,loop)
C
C  ERROR PROCESSING
C
C
      RETURN
      END
