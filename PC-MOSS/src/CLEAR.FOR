      SUBROUTINE clear(iarr,ndim)
C
C *****************************************************************************
C
C             S U B R O U T I N E     C L E A R
C FUNCTION:
C             THIS ROUTINE ZEROES AN ARRAY.
C PARAMETERS:
C             IARR      IN - ARRAY TO BE ZEROED.
C             NDIM      IN - SIZE OF IARR ARRAY.
C ROUTINES
C CALLED:     NONE.
C
C *****************************************************************************
C
      DIMENSION iarr(ndim)
C
C  ZERO AN ARRAY
C
      DO i = 1 , ndim
         iarr(i) = 0
      ENDDO
C
      RETURN
      END
