      SUBROUTINE unpack(ibuff,ndim)
C
C ***********************************************************
C
C              S U B R O U T I N E     U N P A C K
C FUNCTION:
C             THIS ROUTINE UNPACKS AN ARRAY IN S2 FORMAT AND PUT IT IN
C             R1 FORMAT.  THE ARRAY IS UNPACKED FROM TWO CHARACTER PER
C             WORD TO ONE CHARACTER PER WORD BLANK FILLED AND RIGHT
C             JUSTIFIED.
C PARAMETERS:
C             IBUFF     IO  - THE ARRAY TO UNPACK.
C             NDIM      IN  - THE IBUFF SIZE.
C ROUTINES
C CALLED:     NONE.
C
C
C *********************************************************
C
      DIMENSION ibuff(ndim)
      LOGICAL master
C
      istart = ((ndim+1)/2) + 1
      npoint = ndim + 1
C
      IF ( ibuff(1).LT.0 ) master = .TRUE.
C
 100  CONTINUE
      istart = istart - 1
      IF ( istart.LE.0 ) THEN
C
C
         IF ( master ) THEN
            ibuff(1) = (-ibuff(1))
            master = .FALSE.
         ENDIF
         RETURN
      ELSE
C
         ihold = ishft(iabs(ibuff(istart)),8)
         irhgt = ishft(ihold,-8)
         ileft = ishft(iabs(ibuff(istart)),-8)
         npoint = npoint - 1
         ibuff(npoint) = ileft
         npoint = npoint - 1
         ibuff(npoint) = irhgt
         GOTO 100
      ENDIF
      END
