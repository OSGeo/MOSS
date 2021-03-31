 
      SUBROUTINE orderi(x,n)
CC
CC SORTS AN ARAY OF INTEGER IN PLACE, IN ASCENDING ORDER. X IS THE ARRAY,
CC N IS ITS LENGTH. WE USE THE HEAPSORT METHOD IF N IS LARGE, THE SINKING
CC SORT IF N IS SMALL.
C***********************************************************************
CC
      INTEGER x(*)
CC
      IF ( n.LE.1 ) GOTO 300
      IF ( n.GT.40 ) THEN
CC
CC      HEAPSORT
CC
         l = n/2 + 1
         k = n
      ELSE
CC
CC USE A SIMPLE BUBBLE SORT INSTEAD
CC
 50      CONTINUE
         iflag = 0
         DO k = 2 , n
            IF ( x(k).LT.x(k-1) ) THEN
               m = x(k)
               x(k) = x(k-1)
               x(k-1) = m
               iflag = 1
            ENDIF
         ENDDO
         IF ( iflag.EQ.0 ) GOTO 300
         GOTO 50
      ENDIF
 
 100  CONTINUE
      IF ( l.EQ.1 ) THEN
 
         m = x(k)
         x(k) = x(1)
         k = k - 1
         IF ( k.EQ.1 ) THEN
 
            x(1) = m
            GOTO 300
         ELSE
            j = l
         ENDIF
      ELSE
         l = l - 1
         m = x(l)
 
         j = l
      ENDIF
 
 200  CONTINUE
      i = j
      j = j + j
      IF ( j.LT.k ) THEN
 
         IF ( x(j).LT.x(j+1) ) j = j + 1
      ELSEIF ( j.NE.k ) THEN
         x(i) = m
         GOTO 100
      ENDIF
 
      IF ( x(j).LE.m ) THEN
 
         x(i) = m
         GOTO 100
      ELSE
         x(i) = x(j)
         GOTO 200
      ENDIF
 
 
 300  CONTINUE
      RETURN
      END
