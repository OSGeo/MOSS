      SUBROUTINE genral(x1,y1,x2,y2,a,b,c)
C
C *****************************************************************************
C
C             S U B R O U T I N E     G E N R A L
C FUNCTION:
C             GENERATE THE PARAMETERS FOR THE EQUATION
C             OF THE LINE FROM (X1,Y1) TO (X2,Y2)
C             AND RETURN THE PARAMETERS IN (A,B,C)
C PARAMETERS:
C             (X1,Y1),(X2,Y2) IN - THE TWO POINTS NEEDED FOR THE
C                                  LINE EQUATION.
C             A,B,C  OUT - THE PARAMETERS FOR THE LINE EQUATION OF THE
C                          FORM: AX+BY+C=0.
C ROUTINES
C CALLED:     NONE.
C *****************************************************************************
C
      DOUBLE PRECISION a , b , c , dx2 , dy2
      dx2 = dble(x2)
      dy2 = dble(y2)
      a = dble(y1) - dy2
      b = dx2 - dble(x1)
      c = -a*dx2 - b*dy2
C
      RETURN
      END
