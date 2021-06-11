      SUBROUTINE chash(icom,nchars,iprim,ihash,lenr)
      DIMENSION icom(nchars)
C
C *****************************************************************************
C
C             S U B R O U T I N E     C H A S H
C
C  FUNCTION: CALCULATE THE HASH ADDRESS OF A HEAD NODE.  THIS IS
C            A MORE SOPHISTICATED HASHING FUNCTION THAT HASH
C PARAMETERS:
C     ICOM    I- ARRAY CONTAINING KEY
C     NCHARS  I- NUMBER OF CHARACTERS IN KEY
C     IPRIM   I- LENGTH OF HASH TABLE
C     IHASH   O- RETURNED HASH
C     LENR    I- TABLE RECORD LENGTH
C
C ROUTINES
C CALLED:     NONE.
C
C *****************************************************************************
C
      hash = 0.0
      DO i = 1 , nchars
         hash = hash + ((10*i)-10)*icom(i)
      ENDDO
C
      xlen = iprim
      xlen = xlen/float(lenr)
      hashi = hash/xlen
      itemp = hashi
      itemp = (hashi-itemp)*xlen
      ihash = itemp*lenr + 1
C
      RETURN
      END
