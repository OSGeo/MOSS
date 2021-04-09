C
C  PUT A SINGLE VALUE FOR A SINGLE ATTRIBUTE GIVEN A
C  POLYGON ITEM NUMBER
C
      SUBROUTINE putmaf(ichan,mwords,locate,ibuff,item,map,val)
      COMMON /io    / nprnt , ioin
C
      DIMENSION ibuff(128) , map(1) , iv(2)
      INTEGER record
      EQUIVALENCE (rnum,iv(1))
C
C  CALCULATE RECORD IN WHICH VALUE LIES
C
      loop = (abs(mwords)-1)/2 + 1
      IF ( loop.GT.10 ) loop = 10
      itype = mwords
      nwords = iabs(mwords)
      limit = 128/nwords
      record = ((item-1)/limit) + 1
C
C  READ CORRECT BLOCK
C
      CALL rdblk(ichan,record+locate-1,ibuff,1,ier)
      IF ( ier.NE.0 ) THEN
C
C  TRAP EOF WHEN WRITING A BRAND NEW FILE
C
         IF ( ier.NE.3017 ) GOTO 100
         CALL clear(ibuff,128)
      ENDIF
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
         DO i = 1 , loop
            ibuff(index+i-1) = map(i)
         ENDDO
C
C  INTEGER
C
      ELSEIF ( itype.GT.1 ) THEN
C
C  REAL
C
         rnum = val
         ibuff(index) = iv(1)
         ibuff(index+1) = iv(2)
      ELSE
         ibuff(index) = val
      ENDIF
C
C WRITE OUT
C
      CALL wrblk(ichan,record+locate-1,ibuff,1,ier)
C
C  JUMP OUT
C
 100  CONTINUE
      RETURN
      END
