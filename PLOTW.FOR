 
      SUBROUTINE plotw(x,y,ipen)
C
      COMMON /plwrk / xl , yl , x1 , y1 , x2 , y2 , kgnflg
C
      COMMON /pwindo/ xmin , ymin , xmax , ymax , xlast , ylast , lastfg
      COMMON /mdevice/ idev
C
C      DATA XMIN,YMIN,XMAX,YMAX/-9.E10,-9.E10,9.E10,9.E10/
 
C
      IF ( ipen.LT.0 .OR. ipen.EQ.999 ) CALL pendv(x,y,ipen,idev)
      IF ( ipen.LT.0 .OR. ipen.EQ.999 ) RETURN
      IF ( ipen.NE.3 ) THEN
         xl = xlast
         yl = ylast
         CALL zclipt(xl,yl,x,y,x1,y1,x2,y2,kgnflg)
         IF ( kgnflg.NE.1 ) THEN
C/*  LINE OUTSIDE WINDOW
            IF ( x1.NE.xlast .OR. y1.NE.ylast .OR. lastfg.EQ.0 )
     &            CALL pendv(x1,y1,3,idev)
            CALL pendv(x2,y2,ipen,idev)
            xlast = x
            ylast = y
            lastfg = 1
            RETURN
         ENDIF
      ENDIF
      xlast = x
      ylast = y
      lastfg = 0
      RETURN
      END
