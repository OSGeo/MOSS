      SUBROUTINE igetpts(nxy,k,n,uv,isw)
      COMMON /ippy  / ipp(3) , iofx , iofy , iofp , iofq
$Include:'xypnts.500'
$Include:'pqpnts.500'
C
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
C GIVEN A BOUNDARY IN XY CONSISTING OF NXY POINTS, WE EXTRACT N POINTS
C IN SEQUENCE STARTING WITH POINT NUMBER K, AND STORE THESE N POINTS IN
C THE FIRST N POSITION S OF UV. SINCE XY IS A CLOSED BOUNDARY, THE N
C SEQUENTIAL POINTS MAY PASS THROUGH THE LAST POSITION  IN XY AND
C CONTINUE IN THE BEGINNING OF XY.  THIS WE HANDLE AUTOMATICALLY.
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
      DIMENSION uv(2,1)
      IF ( n.LE.0 ) RETURN
      l = k
      IF ( l.EQ.0 ) l = nxy - 1
      DO j = 1 , n
         IF ( l.EQ.nxy ) l = 1
         IF ( isw.EQ.2 ) THEN
            CALL sspoints('P ',p,pliofp,l+iofp,1)
            CALL sspoints('Q ',q,qliofq,l+iofq,1)
            uv(1,j) = pliofp
            uv(2,j) = qliofq
         ELSE
            CALL sspoints('X ',x,xliofx,l+iofx,1)
            CALL sspoints('Y ',y,yliofy,l+iofy,1)
            uv(1,j) = xliofx
            uv(2,j) = yliofy
         ENDIF
         l = l + 1
      ENDDO
      RETURN
      END
