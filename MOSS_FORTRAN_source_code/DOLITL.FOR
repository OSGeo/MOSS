      SUBROUTINE dolitl(nx,ifskip,ier)
C***  THIS SUBROUTINE WAS WRITTEN BY SKRIVAN AND KARLINGER
C***  MODIFIED BY  W.D. GRUNDY, JANUARY, 1980
C***  MODIFIED DOOLITTLE METHOD FOR A SYMMETRIC MATRIX.
C***  REFERENCE "INTRODUCTION TO MATRICES WITH APPLICATIONS IN
C***  STATISTICS", BY F.A. GRAYBILL.
C***  IF P IS SYMMETRIC, THEN P = S*T WHERE T = S'/D AND D IS THE
C***  DIAGONAL MATRIX FROM S.
C***  THE DECOMPOSITION COMPUTES S ONLY WHICH REPLACES P.
C***  P IS ASSUMED A LOWER TRIANGULAR MATRIX STORED BY COLUMNS.
C***  R IS THE RIGHT-HAND-SIDE KNOWN VECTOR.
C***  W IS AN WORKSPACE VECTOR IN WHICH THE SOLUTION WILL BE STORED.
C***  ID IS A POINTER VECTOR GIVING THE LOCATION OF THE DIAGONAL
C***  ELEMENTS IN P.
C***  IFSKIP = 0 FOR DECOMPOSITION, FORWARD AND BACKWARD SUBSTITUTION.
C***         = 1 FOR FORWARD AND BACKWARD SUBSTITUTION ONLY.
C***     IER = 1 IF P(1) = 0. WHEN THE ALGORITHM WILL FAIL.
C***         = 0 OTHERWISE.
      DOUBLE PRECISION sum , p1 , p2
      COMMON /cov4  / p(1830) , id(60) , w(60) , ihol(51) , r(60)
      ier = 0
      n = nx
      IF ( p(1).NE.0 ) THEN
         IF ( ifskip.EQ.0 ) THEN
C***  DECOMPOSITION TO GET LOWER TRIANGULAR S MATRIX
            p(1) = 1./p(1)
            DO j = 2 , n
               DO i = j , n
                  ks = id(i)
                  jm1 = j - 1
                  sum = 0.D0
                  DO k = 1 , jm1
                     js = id(k)
                     is = js + i - k
                     is1 = js + j - k
                     p1 = p(is)
                     p2 = p(is1)
                     IF ( p1.NE.0 ) THEN
                        IF ( p2.NE.0 ) THEN
                           sum = sum + p1*p2*p(js)
                        ENDIF
                     ENDIF
                  ENDDO
                  i1 = id(j) + i - j
                  p(i1) = p(i1) - sum
                  IF ( i.EQ.j ) THEN
                     IF ( p(ks).NE.0 ) THEN
                        p(ks) = 1./p(ks)
                     ELSE
                        ier = 1
                        GOTO 100
                     ENDIF
                  ENDIF
               ENDDO
            ENDDO
         ENDIF
C***  FORWARD SUBSTITUTION FOR SYSTEM S*W = R
         w(1) = r(1)*p(1)
         DO i = 2 , n
            is = id(i)
            sum = 0.
            im1 = i - 1
            DO j = 1 , im1
               js = id(j) + i - j
               sum = sum + w(j)*p(js)
            ENDDO
            w(i) = (r(i)-sum)*p(is)
         ENDDO
C***  BACKWARD SUBSTITUTION FOR SYSTEM T*V = W
C***  WHERE V IS STORED IN W
         DO i = 2 , n
            sum = 0.0D0
            ii = n - i + 1
            js = id(ii)
            iip1 = ii + 1
            DO j = iip1 , n
               iis = js + j - ii
               sum = sum + w(j)*p(iis)
            ENDDO
            w(ii) = w(ii) - sum*p(js)
         ENDDO
      ELSE
         ier = 1
      ENDIF
 100  CONTINUE
      RETURN
      END
