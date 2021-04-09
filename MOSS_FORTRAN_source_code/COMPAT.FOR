C**********************************************************************
C
C     NAME:  SUBROUTINE COMPAT
C
C     FUNCTION:  RETURN AN INTEGER OR REAL ATTRIBUTE VALUE GIVEN THE
C                FEATURE NUMBER AND ATTRIBUTE NUMBER.
C
C     PARAMETERS:  NAME     USE     TYPE     DIMENSION     RANGE
C                  -------------------------------------------------
C                  ICHATT    I      I*2                      INTEGER
C                  CHANNEL TO THE MULTIPLE ATTRIBUTES FILE
C                  -------------------------------------------------
C                  JATTR     I      I*2        8,200         INTEGER
C                  ARRAY CONTAINING ATTRIBUTE NAME, TYPE, LENGTH & FIRST RECORD
C                  -------------------------------------------------
C                  IFEAT     I      I*2                      INTE
C                  FEATURE NUMBER
C                  -------------------------------------------------
C                  IATTR     I      I*2                      INTE
C                  ATTRIBUTE NUMBER
C                  -------------------------------------------------
C                  RNUM      O      R*4                      REAL
C                  ATTRIBUTE VALUE OF THE FEATURE AND ATTRIBUTE
C                  -------------------------------------------------
C                  IER       O      I*2                      INTEGER
C                  ERROR INDICATOR
C                  -------------------------------------------------
C***********************************************************************
C
      SUBROUTINE compat(ichatt,jattr,ifeat,iattr,rnum,ier)
      COMMON /io    / nprnt , ioin
C
      INTEGER ibuff(128) , jattr(13,200) , ir(2)
      EQUIVALENCE (ir(1),val)
C
C  GET LOCATION AND DATA TYPE OF SPECIFIED ATTRIBUTE
C
      itype = jattr(11,iattr)
      nchars = jattr(12,iattr)
      locate = jattr(13,iattr)
      limit = 128/nchars
C
C  CALCULATE LOCATION FOR THIS ITEM IN .AT FILE
C
      IF ( ifeat.GT.1 ) THEN
         irec = ((ifeat-1)/limit) + 1
      ELSE
         irec = 1
      ENDIF
C
C  READ IN ATTRIBUTE RECORD
C
      nrec = irec + locate - 1
      CALL rdblk(ichatt,nrec,ibuff,1,ier)
      IF ( ier.NE.0 ) THEN
C
C  ERROR RETURNS
C
         WRITE (nprnt,99001) ier
99001    FORMAT (
     &         ' *COMPAT* ERROR FROM DSIN READING ATTRIBUTE FILE  ERROR'
     &         ,I5)
      ELSE
C
C  CALCULATE LOCATION IN THIS .AT REC TO RETRIEVE VALUE
C
         base = aint((irec-1)*float(limit))
         index = (((ifeat-base-1)*nchars)+1)
C
         val = float(ibuff(index))
         IF ( itype.EQ.2 ) ir(1) = ibuff(index)
         IF ( itype.EQ.2 ) ir(2) = ibuff(index+1)
C
         rnum = val
      ENDIF
C
      RETURN
      END
