      SUBROUTINE igetis(ibuff,island,nsland,ichan,nrec)
C ****************************************************************
C
C           S U B R O U T I N E    I G E T I S
C
C  FUNCTION: RETURN ISLAND INFORMATION FROM IN CORE RECORD
C
C PARAMETERS:
C       IBUFF  IN - FIXED LENGTH PORTION OF MOSS RECORD
C       ISLAND IN - THE NUMBER OF ISLAND POINTERS
C       NSLAND  IN - NUMBER OF ISLANDS
C       ICHAN IN - MAP FILE CHANNEL OPENER.
C       NREC IN - POLYGON RECORD NUMBER.
C ROUTINES
C CALLED:    RDBLK.
C *****************************************************************
$Include:'IO.inc'
      DIMENSION kbuff(128)
      DIMENSION ibuff(128) , island(1)
      DATA maxis/350/
C
C
      nsland = 0
C
      nsland = ibuff(26)
C
      IF ( nsland.GE.1 ) THEN
C
C  GET POINTERS
C
         ipoint = 26
C
         loop = nsland
         IF ( loop.GT.99 ) loop = 99
         DO i = 1 , loop
            ipoint = ipoint + 1
            island(i) = ibuff(ipoint)
         ENDDO
C
C  CHECK FOR SECOND RECORD OF ISLAND POINTERS
C
         IF ( nsland.GE.100 ) THEN
            IF ( nsland.GT.maxis ) WRITE (nprnt,99001) nsland , maxis
C
C  MORE ISLAND DATA.  READ NEXT RECORD AND STORE IN ISLAND
C
            j = 1
            CALL rdblk(ichan,nrec+j,kbuff,1,ier)
            IF ( ier.NE.0 ) WRITE (nprnt,99002) nrec + j
            ipoint = 0
            DO i = 100 , nsland
               ipoint = ipoint + 1
               island(i) = kbuff(ipoint)
               IF ( ipoint.GE.128 ) THEN
                  j = j + 1
                  CALL rdblk(ichan,nrec+j,kbuff,1,ier)
                  IF ( ier.NE.0 ) WRITE (nprnt,99002) nrec + j
                  ipoint = 0
               ENDIF
            ENDDO
C
C  ERROR RETURN
C
99001       FORMAT (' *IGETIS*  NUMBER OF ISLANDS',I5,' EXCEEDS MAX OF',
     &              I5)
99002       FORMAT (' *IGETIS*  ERROR FROM RDBLK OF RECORD',I5)
         ENDIF
      ENDIF
C
C  RETURN
C
      RETURN
      END
