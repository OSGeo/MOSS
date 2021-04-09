      SUBROUTINE circle(n,xcen,ycen,rad)
$Include:'xypnts.inc'
C
C  GENERATE A PARAMETRIC REPRESENTATION OF A CIRCLE
C
      p = 2.0*3.14156/(n-1)
      c1 = cos(p)
      s1 = sin(p)
      xxt = rad
      yyt = 0.0
      CALL spoints('X ',x,(xxt+xcen),n,2)
c                x(n)=xxt+xcen
      CALL spoints('Y ',y,(yyt+ycen),n,2)
C                 y(n)=yyt+ycen
      CALL spoints('X ',x,xn,n,1)
      CALL spoints('Y ',y,yn,n,1)
      xlast = xn - xcen
      ylast = yn - ycen
      nvert = n
      DO i = 2 , n
         xxt = (xlast)*c1 - (ylast)*s1
         yyt = (xlast)*s1 + (ylast)*c1
         nvert = nvert - 1
         xlast = xxt
         ylast = yyt
         CALL spoints('X ',x,(xxt+xcen),nvert,2)
c                 x(nvert)=xxt+xcen
         CALL spoints('Y ',y,(yyt+ycen),nvert,2)
c                  y(nvert)=yyt+ycen
      ENDDO
C
      RETURN
      END
