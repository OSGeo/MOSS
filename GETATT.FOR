      SUBROUTINE getatt(ichan,mwords,locate,item,jchar,ier)
C *********************************************************************
C
C  SUBROUTINE GETATT
C
C  Purpose:  To retrieve a single attribute given an item number.
C
C
C  Parameters    Use    Type   Dimension       Description
C     ----       ---    ----   ---------   ------------------
C
C     ICHAN      I      INT                 CHANNEL USED FOR MULTIPLE
C                                           ATTRIBUTE FILE
C
C     MWORDS     I      INT                 NUMBER OF WORDS USED TO
C                                           STORE ATTRIBUTE DATA
C
C     LOCATE     I      INT                 FIRST RECORD NUMBER FOR
C                                           ATTRIBUTE DATA
C
C     ITEM       I      INT                 THE ITEM NUMBER TO RETRIEVE
C
C     JCHAR      O      INT        128      THE RETRIEVED ATTRIBUTE
C
C  Files:    Logical Unit       File name or description
C            ------------       ------------------------
C            ICHAN              MULTIPLE ATTRIBUTES FILE
C
C  Calls:  RDBLK
C
C  Called by: SAVE
C
C  Language/System: Fortran V, Data General AOS or AOS/VS
C
C  Written by: Mike Carsella (TGS/BLM)
C
C  Date: August 1985
C
C **********************************************************************
C
      COMMON /io    / nprnt , ioin
      INTEGER ibuff(128) , jchar(128)
      INTEGER record , ichan , mwords , locate , item , limit , base , 
     &        index
C
C       DETERMINE ATTRIBUTE RECORD
C
      limit = 128/mwords
      record = ((item-1)/limit) + 1
C
C       READ CORRECT BLOCK
C
      CALL rdblk(ichan,record+locate-1,ibuff,1,ier)
      IF ( ier.NE.0 ) THEN
C*************************************************************************
C       ERROR PROCESSING
C
         WRITE (nprnt,99001) ier
99001    FORMAT (' *GETATT*  ERROR READING ATTRIBUTE FILE  - ERROR',I5)
      ELSE
C
C       CALCULATE LOCATION IN RECORD FOR ATTRIBUTE
C
         base = aint((record-1)*float(limit))
         index = (((item-base-1)*mwords)+1)
C
C       STUFF JCHAR WITH ATTRIBUTE DATA
C
         DO i = 1 , mwords
            jchar(i) = ibuff(index+i-1)
C
         ENDDO
      ENDIF
C
      RETURN
      END
