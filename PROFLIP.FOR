 
C**********************************************************************
C
C     SUBROUTINE PROFLIP
C
C     FUNCTION:  FLIP-FLOP XDATA,YDATA ARRAYS
C
C     CALLED BY:  PROFIT
C
C*********************************************************************
C
      SUBROUTINE proflip
      COMMON /pvalss/ xdata(512) , ydata(512) , zdata(512) , nvals , 
     &                mxvals
      REAL xdata , ydata , zdata , temp
      INTEGER nvals , mxvals
C
      DO i = 1 , nvals
         temp = xdata(i)
         xdata(i) = ydata(i)
         ydata(i) = temp
      ENDDO
C
      RETURN
      END
