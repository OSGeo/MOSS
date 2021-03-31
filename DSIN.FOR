C******************************************************************************
C
C      NAME:          SUBROUTINE DSIN
C
C      FUNCTION:      RETURN A LOGICAL RECORD
C
C      PARAMETERS:
C            ICHAN      THE FORTRAN CHANNEL NUMBER
C
C               IBUFF   THE ARRAY TO RECEIVE THE RECORD
C
C               REC     THE LOGICAL RECORD NUMBER
C
C               IER     ERROR FLAG   1 IS OK
C                                   -5 IS CHANEL NOT DS-OPENED
C
C
C       1 SYSTEM CHANNEL
C       2 NUMBER OF BLOCKS OR BYTES
C       3 FORTRAN RECORD LENGTH IN WORDS
C       4 # OF FORTRAN RECORDS PER PHYSICAL RECORD
C
C        RDPKT LAYOUT
C
C       1 LOGICAL RECORDS PER PHYSICAL RECORD
C       2 CURRENT RECORD
C
C       IDPKT LAYOUT
C
C       1 # OF BITS OR WORDS PER LOGICAL RECORD
C       2 IBW 1-WORDS 2-BITS...NEG IF WRITTEN ON
C       3 RELATIVE FIRST WORD ADDRESS
C
C******************************************************************************
C
      SUBROUTINE dsin(ichan,ibuff,rec,ier)
      COMMON /cmpkt / ichby(4,64)
      COMMON /dspkt / rdpkt(2,64) , idpkt(3,64)
      COMMON /dsbuf / mlen , idata(8192)
      COMMON /chmn  / minchn
      INTEGER byte , fld , minchn
      INTEGER*4 irec , ifstart , ifend , ipos , i
      DIMENSION ibuff(1)
      DATA smval/.00001/
C
C
C..... CHECK THAT 'ICHAN' HAS BEEN DSOPNed
C
      DO n = 1 , 64
         IF ( ichby(1,n).EQ.ichan ) THEN
C
C       GET RECORD NUMBER & START ADDRESS
C
            ier = 0
            grec = aint(anint(rec-1.)/rdpkt(1,ichan-minchn)) + 1.
            strt = (anint(rec-1.)-((grec-1.)*rdpkt(1,ichan-minchn)))
     &             *idpkt(1,ichan-minchn) + 1.
C
C**     IS LOGICAL RECORD ON CURRENT PHYSICAL RECORD ?
C
            IF ( abs(grec-rdpkt(2,ichan-minchn)).LT.smval ) GOTO 400
C
C**     DUMP CORE RECORD IF WRITTEN & BRING IN NEW
C
            IF ( idpkt(2,ichan-minchn).GE.0 ) GOTO 200
            irec = dint((dble(rdpkt(2,ichan-minchn))-1.D0)
     &             *dble(ichby(2,ichan-minchn)))
            ifstart = irec/ichby(2,ichan-minchn)*ichby(4,ichan-minchn)
     &                 + 1
            ifend = ifstart + ichby(4,ichan-minchn) - 1
            ifunt = ichby(1,ichan-minchn)
            DO i = ifstart , ifend
               ipos = idpkt(3,ichan-minchn) + (i-ifstart)
     &                *ichby(3,ichan-minchn)
               CALL write_f7(ifunt,i,idata(ipos),ichby(3,ichan-minchn),
     &                       ier)
               IF ( ier.NE.0 ) GOTO 100
            ENDDO
            ier = 0
            GOTO 100
         ENDIF
      ENDDO
C..... CHANEL NOT DS-OPENED
      ier = -5
      GOTO 500
 100  CONTINUE
      IF ( ier.NE.0 ) GOTO 500
C
C**     READ NEW RECORD...ASSUMES IT WAS WRITTEN BY OUT OR ABOVE WRITE
C
 200  CONTINUE
      rdpkt(2,ichan-minchn) = grec
      idpkt(2,ichan-minchn) = isign(idpkt(2,ichan-minchn),1)
      irec = dint((dble(rdpkt(2,ichan-minchn))-1.D0)
     &       *dble(ichby(2,ichan-minchn)))
      ifstart = irec/ichby(2,ichan-minchn)*ichby(4,ichan-minchn) + 1
      ifend = ifstart + ichby(4,ichan-minchn) - 1
      ifunt = ichby(1,ichan-minchn)
      icount = 0
      DO i = ifstart , ifend
         ipos = idpkt(3,ichan-minchn) + (i-ifstart)
     &          *ichby(3,ichan-minchn)
         CALL read_f77(ifunt,i,idata(ipos),ichby(3,ichan-minchn),ier)
         IF ( ier.NE.0 ) GOTO 300
         icount = icount + ichby(3,ichan-minchn)
      ENDDO
 300  CONTINUE
      IF ( ier.EQ.-1 ) THEN
C
C**     NULL FILL TO END OF PHYSICAL RECORD FOR EOF READ
C
         ier = 0
         j = idpkt(3,ichan-minchn) + icount
         k = (idpkt(3,ichan-minchn)-1) + ichby(2,ichan-minchn)*256
C          CALL BYTSET(IDATA(IDPKT(3,ICHAN-MINCHN)),ICOUNT*2+1, 0)
         DO i = j , k
            idata(i) = 0
         ENDDO
      ENDIF
      ier = 0
C
C**     RETRIVE LOGICAL RECORD FROM COUMPUTED LOCATION
C
 400  CONTINUE
      IF ( iabs(idpkt(2,ichan-minchn)).EQ.2 ) THEN
C
C**     BIT TRANSFER
C
         iword = (strt-1.)/16.
         ibit = strt - (iword*16.)
         ibuff(1) = fld(idata(idpkt(3,ichan-minchn)+iword),ibit,
     &              ibit+idpkt(1,ichan-minchn)-1)
      ELSE
C
C**     WORD TRANSFER
C
         CALL amove(idpkt(1,ichan-minchn),
     &              idata(idpkt(3,ichan-minchn)+int(strt)-1),ibuff(1))
      ENDIF
C
 500  CONTINUE
      RETURN
      END
