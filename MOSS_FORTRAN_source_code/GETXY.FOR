      SUBROUTINE getxy(xm,alt)
      COMMON /parms / ps , psa(5) , lastm , lastalt , nextm , nextalt , 
     &                lastin , lastout
      COMMON /rundata/ xmmin , altmax , dmachc , daltc , nc , nrnosave , 
     &                 itemid , idisp , ictsub
      LOGICAL nosave
      DATA smval/.00001/
      fract = .5
      dps = psa(lastin+1) - psa(lastin)
      IF ( dps.NE.0.0 ) fract = (ps-psa(lastin))/dps
      IF ( lastin.EQ.2 ) THEN
         falt = fract
         fmach = 0.
      ELSEIF ( lastin.EQ.3 ) THEN
         falt = 1.
         fmach = fract
      ELSEIF ( lastin.EQ.4 ) THEN
         fmach = 1.
         falt = 1. - fract
      ELSE
         fmach = 1. - fract
         falt = 0.
      ENDIF
      alt = altmax - daltc*(falt+float(lastalt-1))
      xm = xmmin + dmachc*(fmach+float(lastm-1))
      IF ( abs(daltc).GT.smval ) alt = alt - daltc/2.
      xm = xm + dmachc/2.
      RETURN
      END
