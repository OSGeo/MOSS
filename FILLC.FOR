      SUBROUTINE fillc(ibuff,nword,iword)
C
C *****************************************************************************
C
C             S U B R O U T I N E     F I L L C
C FUNCTION:
C             THIS ROUTINE FILLS ARRAY IBUFF WITH IWORD CHARACTERS.
C PARAMETERS:
C             IBUFF     OUT- ARRAY TO FILL.
C             NWORD     IN - ARRAY SIZE.
C             IWORD     IN - FILLER CHARACTER.
C ROUTINES
C CALLED:     NONE.
C
C *****************************************************************************
C
      DIMENSION ibuff(*)
C
      DO i = 1 , nword
         ibuff(i) = iword
      ENDDO
C
      RETURN
      END
