      REAL FUNCTION rlnlen(npts)
$Include:'xypnts.inc'
C
C **************************************************************
C
C            F U N C T I O N    R L N L E N
C FUNCTION:
C             THIS ROUTINE CALCULATES THE LENGTH OF A LINE DEFINED BY AN
C             ARRAY OF REAL NUMBERS.
C PARAMETERS:
C             NPTS      IN  - THE NUMBER OF POINTS.
C **************************************************************
C
      rlnlen = 0.0
C
C  DECREMENT NPTS BY ONE
C
      n = npts - 1
C
C  ENTER CALCULATION LOOP
C
      DO i = 1 , n
         CALL spoints('X ',x,xi,i,1)
         CALL spoints('Y ',y,yi,i,1)
         CALL spoints('X ',x,xi1,i+1,1)
         CALL spoints('Y ',y,yi1,i+1,1)
         tlen = (sqrt((xi-xi1)**2+(yi-yi1)**2))/1605.0
         rlnlen = rlnlen + tlen
      ENDDO
C
      RETURN
      END
