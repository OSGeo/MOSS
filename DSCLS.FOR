C******************************************************************************
C
C       NAME:          SUBROUTINE DSCLS
C
C       FUNCTION:      CLOSES A DATA SET OPENED BY DSOPEN
C
C       PARAMETERS:
C               ICHAN   THE FORTRAN CHANNEL NUMBER
C
C               IKPDEL  KEEP/DELETE FLAG
C                       1 - KEEP ( ANYTHING BUT 2 ALSO )
C                       2 - DELETE.
C
C               IER     ERROR FLAG   1 IS OK
C                         32764 - CHANNEL NOT IN USE
C
C       FILES:         FILE SPECIFIED BY ICHAN
C
C       ROUTINES
C       REQUIRED:
C
C       SYSTEM
C       DEPENDENCE:
C
C
      SUBROUTINE dscls(ichan,ier)
C
      COMMON /cmpkt / ichby(4,64)
      COMMON /dspkt / rdpkt(2,64) , idpkt(3,64)
      COMMON /dsbuf / mlen , idata(8192)
      COMMON /chmn  / minchn
      INTEGER*4 ilen , irec , ifstart , ifend , ipos , i
      DATA iwordb/256/
      ikpdel = 1
C
C
C..... CHECK THAT 'ICHAN' HAS BEEN DSOPNed
C
      DO n = 1 , 64
         IF ( ichby(1,n).EQ.ichan ) THEN
C
            ier = 1
C
C**     GET SYSTEM CHANNEL AND SEE IF IT WAS OPENED
C
            IF ( ichby(2,ichan-minchn).EQ.-32767 ) THEN
C
C**     ERROR RETURN
C
               ier = 32764
               GOTO 300
            ELSE
C
C**     DUMP THE CORE RECORD IF IT WAS WRITTEN ON
C
               IF ( idpkt(2,ichan-minchn).GE.0 ) GOTO 200
               ier = 5
               irec = dint((dble(rdpkt(2,ichan-minchn))-1.D0)
     &                *dble(ichby(2,ichan-minchn)))
               IF ( irec.LT.0.0 ) irec = 0.0
               ifstart = irec/ichby(2,ichan-minchn)
     &                   *ichby(4,ichan-minchn) + 1
               ifend = ifstart + ichby(4,ichan-minchn) - 1
               ifunt = ichby(1,ichan-minchn)
               DO i = ifstart , ifend
                  ipos = idpkt(3,ichan-minchn) + (i-ifstart)
     &                   *ichby(3,ichan-minchn)
                  CALL write_f7(ifunt,i,idata(ipos),
     &                          ichby(3,ichan-minchn),ier)
                  IF ( ier.NE.0 ) GOTO 100
               ENDDO
C
               ier = 0
               GOTO 100
            ENDIF
         ENDIF
      ENDDO
C..... CHANEL NOT DS-OPENED
      ier = -5
      GOTO 300
 100  CONTINUE
      IF ( ier.NE.0 ) GOTO 300
C
C**    DO THE CLOSE OPERATION
C
 200  CONTINUE
      CALL closef(ichby(1,ichan-minchn),ikpdel,ier)
      IF ( ier.EQ.0 ) THEN
         ilen = ichby(2,ichan-minchn)*iwordb
         ito = idpkt(3,ichan-minchn)
         ifrom = ito + ilen
         nwords = 0
C
C**     DEASSIGN DATASET PACKET
C
         ichby(1,ichan-minchn) = -32767
         ichby(2,ichan-minchn) = -32767
         rdpkt(1,ichan-minchn) = -32767
         rdpkt(2,ichan-minchn) = -32767
         idpkt(1,ichan-minchn) = -32767
         idpkt(2,ichan-minchn) = -32767
         idpkt(3,ichan-minchn) = -32767
C
C**     UNLESS IT'S THE LAST ONE SHOVE EVERYTHING DOWN INTO THE CLOSE HOLE
C
         DO i = 1 , 63
            IF ( idpkt(3,i).NE.-32767 ) THEN
               IF ( idpkt(3,i).GT.ito ) THEN
                  nwords = nwords + (ichby(2,i)*iwordb)
                  idpkt(3,i) = idpkt(3,i) - ilen
               ENDIF
            ENDIF
         ENDDO
         IF ( nwords.NE.0 ) THEN
C
            CALL amove(nwords,idata(ifrom),idata(ito))
         ENDIF
      ENDIF
C
 300  CONTINUE
      RETURN
      END
