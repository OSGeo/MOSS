      SUBROUTINE setw(x1,y1,x2,y2)
C          SET SCISSOR WINDOW FOR PLOTW
      COMMON /pwindo/ xmin , ymin , xmax , ymax , xlast , ylast , lastfg
      IF ( x1.EQ.x2 .AND. y1.EQ.y2 ) THEN
C          SET WINDOW TO NOT CLIPT ANY DATA
         xmin = -9.E10
         ymin = -9.E10
         xmax = 9.E10
         ymax = 9.E10
         RETURN
      ELSE
C/*  SET ORIGINAL WINDOW
         xmin = x1
         ymin = y1
         xmax = x2
         ymax = y2
         RETURN
      ENDIF
      END
