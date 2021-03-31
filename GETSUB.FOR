      SUBROUTINE getsub(ichan,nrec,ibuff,isub,items)
      DIMENSION ibuff(128)
      DIMENSION isub(15)
C
C ****************************************************************
C
C            S U B R O U T I N E    G E T S U B
C
C
C  FUNCTION:  GIVEN A SUBJECT RECORD NUMBER, RETURN THE
C             ACTUAL SUBJECT (DESCRIPTOR) AND THE NUMBER
C             OF ENTRIES FOR THIS SUBJECT
C
C  INPUT PARAMETERS:
C
C      ICHAN   - DATA CHANNEL ON WHICH MAP MAY BE FOUND
C      NREC    - SUBJECT RECORD NUMBER
C      ISUB    - RETURNED 30 CHARACTER SUBJECT
C      ITEMS   - NUMBER OF ITEMS IN THIS MAP WITH THIS
C                SUBJECT
C      IBUFF   - THE ARRAY CONTAINING THE SUBJECT.
C
C ROUTINES
C **************************************************************
C
C  READ SUBJECT RECORD
C
      CALL rdblk(ichan,nrec,ibuff,1,ier)
C
C  MOVE SUBJECT
C
      DO i = 1 , 15
         isub(i) = ibuff(i)
      ENDDO
C
C  GET NUMBER OF ITEMS
C
      items = ibuff(17)
C
C  AND RETURN
C
      RETURN
      END
