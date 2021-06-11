      SUBROUTINE movxr(ires,ii,ngg,iofx,isw)
      COMMON /vals  / tgr(2,200)
$Include:'xypnts.500'
$Include:'rspnts.500'
$Include:'xrxr.500'
$Include:'xyxy.500'
      DIMENSION ires(1)
      ic = 2839
      kc = 0
      istar = ires(ii)
      istop = ires(ii+1) - 1
      istar = iabs(istar)
      istop = iabs(istop)
      DO i = istar , istop
         kc = kc + 1
         CALL sspoints('XR',xr,xrici21,(ic+i*2-1),1)
         CALL sspoints('XR',xr,xrici2,(ic+i*2),1)
         IF ( isw.EQ.3 ) THEN
            ngg = ngg + 1
c       xy1(ngg)=xr(ic+i*2-1)
c       xy2(ngg)=xr(ic+i*2)
            CALL sspoints('X1',xy1,xrici21,ngg,2)
            CALL sspoints('X2',xy2,xrici2,ngg,2)
         ELSEIF ( isw.EQ.2 ) THEN
c        r(kc)=xr(ic+i*2-1)
c        s(kc)=xr(ic+i*2)
            CALL sspoints('R ',r,xrici21,kc,2)
            CALL sspoints('S ',s,xrici2,kc,2)
         ELSE
c       x(iofx+kc)=xr(ic+i*2-1)
c       y(iofx+kc)=xr(ic+i*2)
            CALL sspoints('X ',x,xrici21,(iofx+kc),2)
            CALL sspoints('Y ',y,xrici2,(iofx+kc),2)
         ENDIF
      ENDDO
C
      IF ( isw.EQ.3 ) RETURN
      ngg = istop - istar + 1
C
      RETURN
      END
