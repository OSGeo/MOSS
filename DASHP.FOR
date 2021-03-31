C
C       XPAGE, YPAGE ARE THE COORIDINATES OF THE POINT TO WHICH THE
C               DASHED LINE IS TO BE DRAWN.
C       DASH IS THE LENGTH OF THE DASH AND SPACE BETWEEN DASHES.
C
C       A DASHED LINE IS DRAWN (INCHES) FROM THE CURRENT PEN POSITION TO
C       THE SPECIFIED XPAGE, YPAGE.  THE SIZE OF THE DASH WILL BE AS
C       CALLED FOR EXCEPT IF THE LINE LENGTH IS LESS THAN DOUBLE THE
C       DASH SIZE THE DASH IS ADJUSTED TO HALF THE LINE LENGTH.
C       DETERMINE CURRENT PEN POSITION.
C
      SUBROUTINE dashp(xpage,ypage,dl)
      CALL where(xt,yt,st)
C       COMPUTE DELTAX AND DELTAY
      dx = xpage - xt
      dy = ypage - yt
      ds = dl
      ic = 2
C       DERIVE THE LINE LENGTH
      s = sqrt(dx*dx+dy*dy)
      IF ( s.GE.0.02*st ) THEN
         ds = ds/s
C       TEST IF LINE LESS THAN DOUBLE DASH LENGTH
         IF ( ds.GT.0.5 ) THEN
C       HALVE DASH LENGTH
            ds = .5
         ENDIF
C       PROPORTION THE DELTAS BY THE LENGTH/DASH RATIO
         dx = dx*ds
         dy = dy*ds
C       SET UP ADJUSTMENT AND END OF LINE TEST FROM ABS GREATEST DELTA
         s = dx
         st = abs(dx) - abs(dy)
         IF ( st.LT.0 ) THEN
            s = dy
         ENDIF
         st = abs(s/ds) - abs(s)
         ds = abs(s)
C
C       DASHED LINE LOOP
C
 50      CONTINUE
         xt = xt + dx
         yt = yt + dy
         st = st - ds
         CALL plotw(xt,yt,ic)
         ic = 5 - ic
         IF ( st.GT.0 ) GOTO 50
      ENDIF
C       LAST SPECIFIC LINE SEGMENT CALL
      CALL plotw(xpage,ypage,ic)
      RETURN
      END
