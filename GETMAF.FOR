C
C  GET A SINGLE VALUE FOR A SINGLE ATTRIBUTE GIVEN A
C  POLYGON ITEM NUMBER
      SUBROUTINE getmaf(ichan,mwords,locate,ibuff,item,map,val)
      COMMON /io    / nprnt , ioin
      DIMENSION ibuff(128) , map(1) , iv(2) , jchar(80)
      INTEGER record
      EQUIVALENCE (rnum,iv(1))
C
C  CALCULATE RECORD IN WHICH VALUE LIES
C
      val = 0
      map(1) = 0
      loop = 1
      itype = mwords
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
         IF ( itype.LT.0 ) THEN
C
C  CHARACTER
C
            loop = nwords
            IF ( loop.GT.80 ) loop = 80
            CALL clear(jchar,loop)
            DO i = 1 , loop
               jchar(i) = ibuff(index+i-1)
            ENDDO
            CALL packc(jchar,map,loop)
C
C  INTEGER
C
         ELSEIF ( itype.GT.1 ) THEN
C
C  REAL
C
            iv(1) = ibuff(index)
            iv(2) = ibuff(index+1)
            val = rnum
         ELSE
            val = ibuff(index)
         ENDIF
C
C  ALL DONE
C
         loop = (nwords-1)/2 + 1
         IF ( loop.GT.10 ) loop = 10
      ENDIF
C
C  JUMP OUT
C
      RETURN
      END
