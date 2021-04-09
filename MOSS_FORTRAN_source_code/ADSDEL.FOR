      SUBROUTINE adsdel(ier,id,index,npts)
C ************************************************
C
C         S U B R O U T I N E     A D S D E L
C
C  FUNCTION - DELETE AN ACTIVE DATA SET FROM THE
C             A&D STRUCTURE TABLE
C
C  INPUT PARAMETERS:
C
C      IER     = ERROR RETURN
C      ID      = ID OF DATA SET TO DELETE
C      INDEX   = POINTER TO FIRST ENTRY IN POINTER
C                TABLE (RETURNED)
C      NPTS    = NUMBER OF ENTRIES IN POINTER TABLE
C                TO DELETE (RETURNED)
C
C
C **************************************************
C
$Include:'WHMAP.inc'
C
C
      npts = istrct(id+1)
C
C  DECREMENT NUMBER OF IDS COUNTER BY ONE
C
      nstrct = nstrct - 7
C
C  IS THIS THE LAST ENTRY IN THE STRUCTURE TABLE ?
C
      IF ( id.NE.nstrct+1 ) THEN
C
C OTHER THAN LAST. COMPRESS STRUCTURE TABLE
C
         jinc = id + 7
         istart = id
         istop = nstrct
C
         DO i = istart , istop
            istrct(i) = istrct(jinc)
            jinc = jinc + 1
         ENDDO
      ENDIF
C
      RETURN
      END
