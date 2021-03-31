      SUBROUTINE icentar(i,xcent,ycent,area1,island,nsland,iconv)
      DIMENSION island(*)
$Include:'xypnts.500'
      DOUBLE PRECISION xj , xl , yj , yl
      DOUBLE PRECISION xmom , ymom , area2
C
      xmom = 0.0
      ymom = 0.D0
      area2 = 0.D0
C
      conv = 4046.856422
      IF ( iconv.EQ.1 ) conv = 43260.
      i = iabs(i)
C
      ipoint = 1
      IF ( nsland.LT.1 ) ipoint = 0
C
      DO j = 2 , i
         IF ( ipoint.NE.0 ) THEN
            IF ( j.EQ.island(ipoint) ) THEN
               ipoint = ipoint + 1
               IF ( ipoint.GT.nsland ) ipoint = 0
               GOTO 100
            ENDIF
         ENDIF
         l = j - 1
         CALL sspoints('X ',x,xll,l,1)
         CALL sspoints('X ',x,xjj,j,1)
         CALL sspoints('Y ',y,yll,l,1)
         CALL sspoints('Y ',y,yjj,j,1)
         xl = dble(xll)
         xj = dble(xjj)
         yl = dble(yll)
         yj = dble(yjj)
         xmom = xmom + (((xl*(xl+xj))+xj**2)*(yl-yj))
         ymom = ymom + (((yl*(yl+yj))+yj**2)*(xl-xj))
         area2 = area2 + ((xl+xj)*(yl-yj))
 100  ENDDO
C
      xcent = sngl(xmom/(3.D0*area2))
      ycent = sngl(ymom/(3.D0*area2))
      ycent = abs(ycent)
      area2 = (dabs(area2/2.D0))
      area1 = area2/dble(conv)
C
      RETURN
      END
