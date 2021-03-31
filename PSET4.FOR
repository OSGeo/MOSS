 
      SUBROUTINE pset4(n,xmax,ymax)
C***  THIS SUBROUTINE WAS WRITTEN BY SKRIVAN AND KARLINGER AND SLIGHTLY
C***  MODIFIED BY W.D. GRUNDY
C***  CALCULATE COEFFICIENTS IN THE SYMMETRIC P MATRIX WHICH WILL
C***  CONSIST OF THE LOWER TRIANGULAR PORTION STORED BY COLUMNS.
C***  ID IS THE POINTER VECTOR GIVING THE LOCATION OF THE DIAGONAL
C***  ELEMENTS IN P.
C***  SUBPROGRAMS REQUIRED: COVAR4,TERM
$INCLUDE: 'MYST.INC'
      COMMON /cov4  / p(1830) , id(60) , w(60) , ihol(51) , r(60)
      DIMENSION p1(2) , p2(2)
      INTEGER*2 ex , ey
      ntot = n + nt
      np1 = n + 1
C***  ZERO OUT ELEMENTS OF P-MATRIX NEEDED FOR KRIGING
      maxpt = ntot*(ntot+1)/2
      DO i = 1 , maxpt
         p(i) = 0.
      ENDDO
C***  CALCULATE THE POINTERS FOR ID
      id(1) = 1
      DO i = 2 , ntot
         id(i) = id(i-1) + ntot + 2 - i
      ENDDO
C***  CALCULATE THE COEFFICIENTS OF THE COVARIANCE OF POINTS I AND J
      DO j = 1 , n
         js = id(j) - j
         jsi = ihol(j)
         p2(1) = xe(jsi)
         p2(2) = yn(jsi)
         DO i = j , n
            is = ihol(i)
            p1(1) = xe(is)
            p1(2) = yn(is)
            p(js+i) = covar4(p1,p2)
         ENDDO
      ENDDO
C***  CALCULATE COEFFICIENTS FROM TERMS OF THE DRIFT
      DO j = 1 , n
         js = id(j) - j
         jsi = ihol(j)
         DO i = np1 , ntot
            m = i - n
            p(js+i) = term(xe(jsi),ex(m),yn(jsi),ey(m),xmax,ymax)
         ENDDO
      ENDDO
C***  ADD MEASUREMENT ERRORS TO THE DIAGONAL COEFFICIENTS
C***  THIS ROUTINE ELIMINTATED FOR MOSS VERSION
C     DO 50 I=1,N
C     ISP=ID(I)
C     ISI=IHOL(I)
C     P(ISP)=P(ISP)+ERMEAS(ISI)
C  50 CONTINUE
      RETURN
      END
