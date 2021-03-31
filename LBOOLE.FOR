      SUBROUTINE lboole(npts1,npts2,inder)
C***********************************************************************
C
C  POLYGON/LINE OVERLAY
C
C***********************************************************************
$Include:'pqpnts.inc'
$Include:'rspnts.inc'
      COMMON /io    / nprnt , ioin
      COMMON /dot   / nslands , jh , ids(1)
      COMMON /offst / sxmin , symin
      COMMON /osave / isub(15) , nlines , idum1 , idum2 , idum3 , ictsub
      DIMENSION xtt(2) , ibuff(128)
      DIMENSION xa(300) , ya(300) , icat(5)
      LOGICAL idum3
      DATA icat/5*'  '/
C
C  NLINES = NUMBER OF INTERSECTED LINES.
C  CURRENT MAX. NUMBER OF INTERSECTIONS PER LINE = 300
C  NPTS3 = OUTPUT NUMBER OF COORDS PER NEW SEGMENT
C
C
C  INITIALIZE
      npts3 = 0
      ind = 0
      inder = 0
      CALL spoints('P ',p,xp2,1,1)
      CALL spoints('Q ',q,yp2,1,1)
c        XP2=P(1)
c        YP2=Q(1)
C  LOOP # LINE SEGMENTS
      DO is = 2 , npts2
         xp1 = xp2
         yp1 = yp2
         CALL spoints('P ',p,xp2,is,1)
         CALL spoints('Q ',q,yp2,is,1)
c        XP2=P(IS)
c        YP2=Q(IS)
C          FIND ALL INTERSECTIONS BETWEEN (XP1,YP1)-(XP2,YP2) & POLYGON
         CALL cros(xp1,yp1,xp2,yp2,npts1,xa,ya,nseg)
C          CHECK TO SEE IF EACH SEGMENT IS INSIDE/OUTSIDE POLYGON
         IF ( nseg.NE.0 ) THEN
C          (XP1,YP1) & (XP2,YP2) SAME POINT
            DO iseg = 1 , nseg
               xp = (xa(iseg)+xa(iseg+1))/2.
               yp = (ya(iseg)+ya(iseg+1))/2.
               ind1 = inpoly(nslands,ids,xp,yp)
               IF ( ind1.NE.1 ) ind1 = 0
C  0 = OUTSIDE = IGNORE
               IF ( ind1.NE.0 ) THEN
C          INSIDE, IF LAST SEG WAS OUTSIDE, SET NEW SEGMENT (-X)
                  IF ( ind.EQ.0 ) THEN
                     npts3 = npts3 + 1
                     CALL spoints('R ',r,-(xa(iseg)+sxmin),npts3,2)
c                  r(npts3)=-(xa(iseg)+sxmin)
                     CALL spoints('S ',s,(ya(iseg)+symin),npts3,2)
                  ENDIF
C                  s(npts3)= ya(iseg)+symin
                  npts3 = npts3 + 1
                  CALL spoints('R ',r,(xa(iseg+1)+sxmin),npts3,2)
C                    r(npts3)=xa(iseg+1)+sxmin
                  CALL spoints('S ',s,(ya(iseg+1)+symin),npts3,2)
               ENDIF
c                    s(npts3)=ya(iseg+1)+symin
               ind = ind1
            ENDDO
         ENDIF
      ENDDO
C          ALL SEGMENTS FOUND
      IF ( npts3.GT.0 ) inder = 1
C  NO SEGMENTS INSIDE POLYGON
      IF ( npts3.EQ.0 ) RETURN
C          OUTPUT EACH LINE SEGMENT FOUND
      low = 1
      nseg = 0
      CALL spoints('R ',r,-1.0,(npts3+1),2)
c                  r(npts3+1)=-1
C        CALL PUTRR(NPTS3+1, -1.)       ;  SET TO COMPLETE LAST SEGMENT
      DO i = 2 , npts3 + 1
C  CONTINUE LINE
         IF ( r(i).LE.0. ) THEN
C  LINE IS (LOW-IHI)
            ihi = i - 1
C  # DATA POINTS
            n = ihi - low + 1
            CALL spoints('R ',r,(abs(r(low))),low,2)
c     r(low)=abs(r(low))   fix seg. flag
            IF ( n.GT.1 ) THEN
C ADDED FOLLOWING LINE TO WRITE TO 'TEMPSUB'  DANNY ALBERTS 9-9-83
               nseg = nseg + 1
               nlines = nlines + 1
               DO l = 1 , 15
                  ibuff(l) = isub(l)
               ENDDO
               CALL wrblk(ictsub,nlines,ibuff,1,ier)
               WRITE (18) isub , icat , n , 0
               DO j = low , ihi
                  CALL spoints('R ',r,xtt(1),j,1)
                  CALL spoints('S ',s,xtt(2),j,1)
C        XTT(1)=R(J)
C        XTT(2)=S(J)
                  WRITE (18) xtt(1) , xtt(2)
               ENDDO
            ELSE
               WRITE (*,*) ' LINE WITH ONLY 1 DATA POINT IN LBOOLE'
            ENDIF
            low = i
         ENDIF
      ENDDO
      RETURN
      END
