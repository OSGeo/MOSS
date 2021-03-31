      SUBROUTINE barc(nxt,npts,ang1,ang2,rad,xcen,ycen)
$Include:'rspnts.inc'
C      dimension r(1),s(1)
C
C  FUNCTION:  DRAW AN ARC OF RADIUS RAD BETWEEN ANG1 AND ANG2 DEFINED BY
C             NPTS POINTS CENTERED AT XCEN,YCEN.
      IF ( ang1.EQ.ang2 ) ang1 = ang2 + 360.
C
C
      IF ( ang1.GE.0.0 ) THEN
         ang1 = ang1 - 360.
      ENDIF
      np = npts + 1
      DO k = 1 , npts
         ang = ang1 + (ang2-ang1)*float(k-1)/npts
         th = ang*.0174533
         np = np - 1
c ipos = position in r and s of where to place arc point
         ipos = nxt + np
         rt = rad*cos(th) + xcen
         st = rad*sin(th) + ycen
         CALL spoints('R ',r,rt,ipos,2)
         CALL spoints('S ',s,st,ipos,2)
      ENDDO
C
      RETURN
      END
