      SUBROUTINE scalit(xorg,yorg,xxlen,yylen,xinc,yinc,xr,yr,dimen)
      REAL ix1 , iy1 , ix2 , iy2
C
C  SET SCALE FACTORS FOR DATABASE TO VIRTUAL COORDINATES
C
      CALL whwnd(ix1,iy1,ix2,iy2)
C
      xr = ix2 - ix1
      yr = iy2 - iy1
      xorg = ix1
      yorg = iy1
      IF ( xr.GT.yr ) THEN
         yylen = dimen
         xxlen = xr/yr
         xxlen = xxlen*dimen
      ELSE
         xxlen = dimen
         yylen = yr/xr
         yylen = yylen*dimen
      ENDIF
      xinc = xxlen/xr
      yinc = yylen/yr
C
      RETURN
      END
