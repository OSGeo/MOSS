      SUBROUTINE crosy(x1,y1,x2,y2,x3,y3,x4,y4,x,y,nix)
      DOUBLE PRECISION a , b , c , p , q , r , d
C
C DO TWO LINES CROSS ONE ANOTHER ?
C
      nix = 1
      IF ( x1.EQ.x2 .AND. y1.EQ.y2 ) RETURN
      IF ( x1.EQ.x3 .AND. y1.EQ.y3 .OR. x2.EQ.x3 .AND. y2.EQ.y3 ) RETURN
      IF ( x1.EQ.x4 .AND. y1.EQ.y4 .OR. x2.EQ.x4 .AND. y2.EQ.y4 ) RETURN
      xmin = amax1(amin1(x1,x2),amin1(x3,x4))
      ymin = amax1(amin1(y1,y2),amin1(y3,y4))
      xmax = amin1(amax1(x1,x2),amax1(x3,x4))
      ymax = amin1(amax1(y1,y2),amax1(y3,y4))
      CALL genral(x1,y1,x2,y2,a,b,c)
      CALL genral(x3,y3,x4,y4,p,q,r)
      CALL cross1(a,b,c,p,q,r,x,y,d,ier)
      IF ( ier.EQ.0 ) RETURN
      IF ( x.GT.xmin .AND. x.LT.xmax .AND. y.GE.ymin .AND. y.LE.ymax )
     &      nix = 2
C
      RETURN
      END
