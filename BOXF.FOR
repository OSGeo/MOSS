 
C***************************************************************************
C
C  FILE:     S U B R O U T I N E   B O X F
C
C  PARAMETERS:
C            IPSS - INPUT - CONTOUR INTERVALS ARRAY
C            NPS  - INPUT - NUMBER OF INTERVALS
C
C  EXTERNAL ROUTINES:
C            FGRID,  IGRIDP, IGRIDS
C
C  SYSTEM DEPENDENTS:
C            IBCLR
C
C******************************************************************************
C
C
      SUBROUTINE boxf(ipss,nps)
$include:'xypnts.inc'
      COMMON /io    / nprnt , ioin
      COMMON /rundata/ xmmin , altmax , dmachc , daltc , nmach , nalt , 
     &                 nosave , itemid , idisp , ictsub
      DIMENSION ipss(128) , rp(5) , icross(16)
C
C.....CALCULATE NUMBER OF WORDS REQUIRED FOR NUMBER OF INTERVALS
      i4off = (nps+15)/16
C
C.....NUMBER OF WORDS TO READ AND WRITE TO INTERVAL FILE
      icross(2) = 2
      IF ( i4off.EQ.2 ) icross(2) = 4
      IF ( i4off.GT.2 .AND. i4off.LT.5 ) icross(2) = 8
      IF ( i4off.GT.4 ) icross(2) = 16
C
C.....OPEN GRID FLAGS FILE
      icross(1) = -1
      CALL igridp(0,0,icross)
C
C.....LOOP ON ROWS
      DO ialt = 2 , nalt
         rp(4) = rgrid(1,ialt-1)
         rp(3) = rgrid(1,ialt)
         DO imach = 2 , nmach
            DO i = 1 , 16
               icross(i) = 0
            ENDDO
            rp(1) = rp(4)
            rp(5) = rp(4)
            rp(2) = rp(3)
            rp(4) = rgrid(imach,ialt-1)
            rp(3) = rgrid(imach,ialt)
            ipslow = min(rp(1),rp(2),rp(3),rp(4))
C.........NOT GOOD DATA POINT
            IF ( ipslow.GT.0 ) THEN
               DO ips = 1 , nps
                  kps = ipss(ips)
                  nt = 0
                  rps = float(kps)
                  DO i = 1 , 4
                     IF ( (rps.GE.rp(i) .AND. rps.LT.rp(i+1)) .OR. 
     &                    (rps.GE.rp(i+1) .AND. rps.LT.rp(i)) )
     &                     nt = nt + 1
                  ENDDO
                  IF ( nt.GE.2 ) THEN
                     nbit = mod(ips,16) - 1
                     IF ( nbit.LT.0 ) nbit = 15
                     iwrd = (ips+15)/16
                     icross(iwrd) = ibset(icross(iwrd),nbit)
                     IF ( nt.GE.4 ) THEN
C.................IF BOX HAS 4 ENTRANCES, PUT IN UPPER HALF OF BIT ARRAY ALSO
                        i4x = iwrd + i4off
                        icross(i4x) = ibset(icross(i4x),nbit)
                     ENDIF
                  ENDIF
               ENDDO
            ENDIF
            CALL igridp(imach-1,ialt-1,icross)
         ENDDO
      ENDDO
C
C.....BEGIN SEARCHING FOR CONTOUR LINE IPS
C
      IF ( idisp.EQ.1 ) CALL newpag
      i0 = 0
      i1 = 1
      i2 = 2
      i3 = 3
      i4 = 4
      CALL igrids(i2,nmach,i2,i2,i1,ipss,nps)
      CALL igrids(i2,i2,i3,nalt,i2,ipss,nps)
      CALL igrids(i2,nmach,nalt,nalt,i3,ipss,nps)
      CALL igrids(nmach,nmach,i3,nalt,i4,ipss,nps)
      CALL igrids(i3,nmach-1,i3,nalt-1,i0,ipss,nps)
C
C.....DELETE GRID FLAGS FILE
      icross(1) = -2
      CALL igridp(0,0,icross)
      IF ( idisp.EQ.1 ) THEN
         CALL flsh
         CALL toutpt(31)
         CALL toutpt(29)
         CALL toutpt(32)
         CALL plout(0,0,-1)
      ENDIF
      RETURN
      END
