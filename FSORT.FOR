      SUBROUTINE fsort(x,ipoint,n)
C***
C***  THIS IS A FAST ROUTINE FOR SORTING A SET OF POINTERS
C***  CONTAINED IN ARRAY IPOINT BASED UPON INCREASING VALUES
C***  IN ARRAY X.  THIS SORT IS NOT STABLE.
C***  ROUTINE ORIGINALLY WRITTEN BY CALVIN SMITH, U. S. ATOMIC
C***  ENERGY COMMISSION, GRAND JUNCTION OFFICE, FOR A CDC 3100
C***  COMPUTER.
C***  RECODED FOR IBM PERSONAL COMPUTER BY W. D. GRUNDY APRIL 1984
C***
C***
C***  DIMENSION REQUIREMENTS:
C***  IF X IS DIMENSIONED AS X(M) IN THE MAIN PROGRAM, SET
C***  DIMENSIONS IU(N) AND IL(N) SUCH THAT 2**(N+1)-1 IS
C***  GREATER THAN OR EQUAL TO M.
C***
      DIMENSION x(1) , ipoint(1) , iu(15) , il(15)
      i = 1
      m = 1
      j = n
 100  CONTINUE
      IF ( i.GE.j ) GOTO 500
 200  CONTINUE
      k = i
      l = j
      ij = (i+j)/2
      IF ( x(ipoint(i)).GT.x(ipoint(ij)) ) THEN
         itemp = ipoint(i)
         ipoint(i) = ipoint(ij)
         ipoint(ij) = itemp
      ENDIF
      IF ( x(ipoint(j)).LT.x(ipoint(ij)) ) THEN
         itemp = ipoint(j)
         ipoint(j) = ipoint(ij)
         ipoint(ij) = itemp
         IF ( x(ipoint(ij)).LT.x(ipoint(i)) ) THEN
            itemp = ipoint(i)
            ipoint(i) = ipoint(ij)
            ipoint(ij) = itemp
         ENDIF
      ENDIF
 300  CONTINUE
      l = l - 1
      IF ( x(ipoint(ij)).LT.x(ipoint(l)) ) GOTO 300
 400  CONTINUE
      k = k + 1
      IF ( x(ipoint(k)).LT.x(ipoint(ij)) ) GOTO 400
      IF ( k.LT.l ) THEN
         itemp = ipoint(l)
         ipoint(l) = ipoint(k)
         ipoint(k) = itemp
         IF ( ij.EQ.l ) THEN
            ij = k
         ELSEIF ( ij.EQ.k ) THEN
            ij = l
         ENDIF
         GOTO 300
      ELSEIF ( k.EQ.l ) THEN
         k = k + 1
         l = l - 1
      ENDIF
      IF ( (l-i).LE.(j-k) ) THEN
         il(m) = k
         iu(m) = j
         j = l
         m = m + 1
      ELSE
         il(m) = i
         iu(m) = l
         i = k
         m = m + 1
      ENDIF
      GOTO 600
 500  CONTINUE
      m = m - 1
      IF ( m.EQ.0 ) RETURN
      i = il(m)
      j = iu(m)
 600  CONTINUE
      IF ( (j-i).GE.11 ) GOTO 200
      IF ( i.EQ.1 ) GOTO 100
 700  CONTINUE
      IF ( i.EQ.j ) GOTO 500
      IF ( x(ipoint(i)).LE.x(ipoint(i+1)) ) THEN
         i = i + 1
         GOTO 700
      ELSE
         k = i
 750     CONTINUE
         itemp = ipoint(k)
         ipoint(k) = ipoint(k+1)
         ipoint(k+1) = itemp
         k = k - 1
         IF ( x(ipoint(k+1)).LT.x(ipoint(k)) ) GOTO 750
         i = i + 1
         GOTO 700
      ENDIF
      END
