      SUBROUTINE getwnd(index,wind,ichan)
C
C **********************************************************
C
C           S U B R O U T I N E   G E T W N D
C
C  FUNCTION: RETURN MBR FOR A DATA ITEM
C
C  INPUT PARAMETERS:
C      INDEX  - LOCATION OF RECORD
C      WIND   - RETURNED MBR
C      ICHAN  - DATA CHANNEL ON WHICH DATA IS LINKED
C ROUTINES
C CALLED:    RDBLK.
C
C ********************************************************
      COMMON /io    / nprnt , ioin
      DIMENSION wind(4)
$Include:'work.inc'
      EQUIVALENCE (xmin,ibuff(15)) , (xmax,ibuff(17))
      EQUIVALENCE (ymin,ibuff(19)) , (ymax,ibuff(21))
C
C  READ CURRECT BLOCK
C
      CALL rdblk(ichan,index,ibuff,1,ier)
      IF ( ier.NE.0 ) THEN
C
C  ERROR RETURN
C
         WRITE (nprnt,99001) index
99001    FORMAT (' *GETWND*  ERROR FROM RDBLK OF RECORD',I5)
      ELSE
C
C  TRANSFER WINDOW
C
         wind(1) = xmin
         wind(2) = xmax
         wind(3) = ymin
C
         wind(4) = ymax
      ENDIF
C
      RETURN
      END
