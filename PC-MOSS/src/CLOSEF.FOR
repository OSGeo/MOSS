C******************************************************************************
C
C             S U B R O U T I N E     C L O S E F
C FUNCTION:
C             THIS ROUTINE CLOSES DISK FILES.
C PARAMETERS:
C             ICHAN     IN  - THE CHANNEL NUMBER TO CLOSE
C             IDELT     IN  - THE FILE TYPE,
C                             1 - CLOSE, RETAIN FILE
C                             2 - CLOSE, DELETE FILE
C             IER       OUT - THE ERROR RETURN FOR WRITING TO, READING,
C                             OPENING, OR CLOSING A FILE,
C                             IER = 1, IF NO ERROR.
C
C NOTE:     THE PARAMETER "MINCHN" IS SET BY BLOCKDATA BECAUSE THE PRIME
C           RESERVES THE FIRST "N" CHANNELS FOR IT'S OWN USE.  MOST
C           OTHER SYSTEMS WON'T NEED THIS.
C
C******************************************************************************
C
      SUBROUTINE closef(ichan,idelt,ier)
C
      COMMON /chmn  / minchn
      CHARACTER*12 cstat
      INTEGER*4 lun4 , ios4
C
C.....CHECK FOR A VALID CHANNEL (NOTHING BELOW MINCHN)
      ier = 0
      IF ( ichan.GT.minchn ) THEN
         lun4 = ichan
         IF ( idelt.EQ.2 ) THEN
            cstat = 'DELETE'
         ELSE
            cstat = 'KEEP'
         ENDIF
         CLOSE (lun4,STATUS=cstat,IOSTAT=ios4)
C....... LET CHANNEL MANAGER KNOW UNIT IS FREE
         ioc = 2
      ENDIF
C
      RETURN
      END
