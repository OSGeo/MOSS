      SUBROUTINE cross1(a,b,c,p,q,r,x,y,d,ier)
C
C *****************************************************************************
C
C             S U B R O U T I N E     C R O S S 1
C FUNCTION:
C             DETERMINE IF LINE (A,B,C) INTERSECTS
C             LINE (P,Q,R).  IF IT DOES, X AND Y
C             ARE THE VALUES.  IF IT DOES NOT, IER
C             IS SET TO 1
C PARAMETERS:
C             A,B,C        IN - LINE AX+BY+C=0.
C             P,Q,R        IN - LINE PX+QY+R=0.
C             (X,Y)        OUT- THE INTERSECTION POINT.
C             D            OUT- BOTTOM DIVISOR MATRIX.
C             IER          OUT- ERROR RETURN
C                               1,IF AN ERROR.
C                               0,IF NO ERROR.
C *****************************************************************************
C
      DOUBLE PRECISION a , b , c , p , q , r , d
      ier = 1
C
      d = a*q - b*p
C
C  LINES PARALELL ?
C
      IF ( dabs(d).EQ.0.D0 ) THEN
         ier = 0
C
         RETURN
      ELSE
C
         x = sngl((b*r-c*q)/d)
         y = sngl((c*p-a*r)/d)
C
         RETURN
      ENDIF
      END
