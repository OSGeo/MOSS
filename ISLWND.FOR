      SUBROUTINE islwnd(isw,ih,ids)
      DIMENSION ids(1)
$Include:'io.inc'
$Include:'xypnts.inc'
$Include:'pqpnts.inc'
$Include:'xrxr.inc'
C
C CALCULATE ISLAND MBR'S
C
      kp = 0
      IF ( isw.EQ.2 ) kp = 1419
C
      DO i = 1 , ih
         xmin = 9999999.
         ymin = xmin
         xmax = -xmin
         ymax = -ymin
         istar = ids(i)
         istop = ids(i+1) - 1
         IF ( ih.EQ.i ) istop = ids(i+1)
         DO j = istar , istop
            IF ( isw.EQ.2 ) THEN
               CALL spoints('P ',p,pj,j,1)
               xmin = amin1(pj,xmin)
               xmax = amax1(pj,xmax)
               CALL spoints('Q ',q,qj,j,1)
               ymin = amin1(qj,ymin)
               ymax = amax1(qj,ymax)
            ELSE
               CALL spoints('X ',x,xj,j,1)
               xmin = amin1(xj,xmin)
               CALL spoints('Y ',y,yj,j,1)
               ymin = amin1(yj,ymin)
               xmax = amax1(xj,xmax)
               ymax = amax1(yj,ymax)
            ENDIF
         ENDDO
         kp = kp + 1
         CALL spoints('XR',xr,xmin,kp,2)
c                    xr(kp)=xmin
         kp = kp + 1
         CALL spoints('XR',xr,xmax,kp,2)
c                    xr(kp)=xmax
         kp = kp + 1
         CALL spoints('XR',xr,ymin,kp,2)
c                    xr(kp)=ymin
         kp = kp + 1
         CALL spoints('XR',xr,ymax,kp,2)
c                    xr(kp)=ymax
      ENDDO
C
      RETURN
      END
